package erbs
package parser

import ast._
import util._
import token.OperatorToken

class ExtendableParser extends RubyParser with OperatorToken with ParserMap with ParserErrors {
  protected val DEFAULT_TAG = "origin"
  protected val hmap: ErbsOpMap[Expr] = new ErbsOpMap
  private val HOST_OPERATORS = Map(
    "+" -> t_plus, "-" -> t_minus, "*" -> t_mul, "/" -> t_div,
    "&&" -> t_and, "||" -> t_or, ">=" -> t_ge, ">" -> t_gt, "<=" -> t_le, "<" -> t_lt
  )

  override def stmnts: Parser[Stmnts] = midStatmnts ^^ { _.prependExpr(hmap.toModule) }
  protected def midStatmnts: Parser[Stmnts] = ((defop | stmnt) <~ (EOL | ";")).* ^^ Stmnts
  override def reserved = K_OPERATOR | K_DEFS | K_AT_TOKEN | super.reserved
  // Parse each items of syntax interleaved by '\s'
  protected lazy val v: PackratParser[String] = """[^()\s]+""".r ^^ identity

  protected lazy val tagBinary: PackratParser[Expr] = tagExpr ~ tagExprR.* ^^ { case f ~ e => DNFBuilder.build(makeBin(f, e)) }
  protected lazy val tagExprR: PackratParser[(Op, Expr)] = tagOp ~ tagExpr ^^ { case op ~ f => (op, f) }
  protected lazy val tagOp: PackratParser[Op] = t_and | t_or
  protected lazy val tagLVar: PackratParser[Expr] = "@token(" ~> v <~ ")" ^^  (ATToken(_)) | lvar
  protected lazy val tagExLVar: PackratParser[Expr] = "!" ~> tagLVar ^^ (Unary(EXT, _))
  protected lazy val tagExpr: PackratParser[Expr] = tagExLVar | tagLVar  | "(" ~> tagBinary <~ ")"

  protected lazy val tagSymbolKey: PackratParser[String] = lvar <~ ":" ^^ (_.v)
  protected lazy val tagKeyValue: PackratParser[Map[String, Expr]] = tagSymbolKey ~ tagBinary ^^ { case k ~ v => Map(k -> v) }
  protected lazy val tagHashBody: PackratParser[Map[String, Expr]] = rep1sep(tagKeyValue, ",") ^^ { _.reduceLeft { (acc, e) => acc ++ e } }

  protected lazy val opTagsDefinition: PackratParser[Map[String, Expr]] = "(" ~> tagHashBody.? <~ ")" ^^ { _.getOrElse(Map.empty) }
  protected lazy val opSyntax: PackratParser[Syntax] =  v.+ ~ opTagsDefinition.? ^^ { case list ~ tags => Syntax(tags.getOrElse(Map.empty), list) }
  protected lazy val opSemantics: PackratParser[Stmnts] = midStatmnts
  protected lazy val opTags: PackratParser[Set[String]] = formalArgs ^^ { _.args.map { case SimpleArgElement(v) => v.v }.toSet }
  protected lazy val opDefinition: PackratParser[(Syntax, Stmnts)] = "defs" ~> opSyntax ~ opSemantics <~ "end" ^^ { case syntax ~ body => (syntax, body) }
  protected lazy val defop: PackratParser[Operators] = "Operator" ~> opTags ~ opDefinition.+ <~ "end" ^^ { case tags ~ d => extendSyntax(tags, d) }

  protected def extendSyntax(tags: Set[String], definitions: Seq[(Syntax, Stmnts)]): Operators = {
    val operators = Operators(tags, definitions)

    // register
    for (op <- operators.ops) { hmap.put(op.tags, ErbsOperator(op, () => buildParser(op))) }

    // This method should be call after calling registerOperators
    extendHostRule(operators.ops.filter(_.tags.contains(DEFAULT_TAG)))
    operators
  }

  protected def extendHostRule(ops: Seq[Operator]) = for (op <- ops) {
    hmap.getWithAllMatch(op.tags).foreach { p =>
      val oldStmnt = stmnt // Creating a variable is by design
      stmnt = p | oldStmnt
    }
  }

  protected def buildParser(op: Operator, context: Context = new Context()): PackratParser[Expr] =
    opToParsers(op, context).reduceLeft { (acc, v) => acc ~ v ^^ { case m1 ~ m2 => m1 ++ m2 } } ^^ op.toMethodCall

  protected def opToParsers(op : Operator, context: Context) = op.terms.map {
    case Right(term) => term ^^^ Map.empty[String, Expr]
    case Left((name, nonTerm)) => findParser(nonTerm, context) match {
      case Some(p) => p ^^ { ast => Map(name -> ast) }
      case None => throw new NoSuchParser(s"$nonTerm (tags of ${PrettyPrinter.call(nonTerm)}) in ${op.syntaxBody}")
    }
  }

  object OnlyDefaultSet { def unapply(s: Set[String]): Boolean = s == Set(DEFAULT_TAG) }

  protected def findParser(cond: Expr, context: Context): Option[PackratParser[Expr]] =
    context.fold(cond)(e => DNFBuilder.build(Binary(AND, cond, e))) match {
      case ATToken(key)        => None
      case LVar(DEFAULT_TAG)   => Some(stmnt)
      case LVar(key)           => hmap.get(key)
      case Unary(EXT, LVar(e)) => hmap.getNot(e)
      case Binary(OR, l, r)    => for (e1 <- findParser(l, context); e2 <- findParser(r, context)) yield { e1 | e2 }
      case e@Binary(AND, _, _) => collectTags(e) match {
        case (OnlyDefaultSet(), EmptySet()) => if (context.isEmpty) Some(stmnt) else buildNewStmnt(context)
        case (t, nt) => (collectTokens(e), context.isEmpty) match {
          case ((EmptySet(), EmptySet()), true) => hmap.getWithAllMatch(t, nt)
          case (c, empty) => {
            val newContxt = context.cloneWith(c)
            val hs = hmap.getParsers(newContxt, t, nt)
            val v = if (empty) hs.map { p => buildParser(p.operator, newContxt) } else hs.map(_.parser())
            v.reduceLeftOption { (acc, v) =>  acc | v }
          }
          case invalid => throw new InvalidCondition(s"Tag requires at least one:  ${invalid.toString}")
        }
      }
      case invalid => throw new InvalidCondition(s"Invalid tag conditions: ${invalid.toString}")
    }

  private def buildNewStmnt(context: Context) = {
    val ep = new ExtendableParser()
    // Now, new parser can't used an existed rule
    // hmap.get(DEFAULT_TAG) match {
    //   case Some(p) => {
    //     val v:  PackratParser[Expr] = p | ep.stmnt.asInstanceOf[PackratParser[Expr]]
    //     ep.stmnt = v.asInstanceOf[ep.PackratParser[Expr]]
    //   }
    //   case None => throw new NoSuchParser("")
    // }
    ep.operator = context.collect(HOST_OPERATORS).values.reduceLeft { (acc, e) => acc | e }.asInstanceOf[ep.PackratParser[Op]]
    Some(ep.stmnt.asInstanceOf[PackratParser[Expr]])
  }

  protected def collectTokens(e: Expr): (Set[String], Set[String]) = e match {
    case Binary(AND, e1, e2) => (collectTokens(e1), collectTokens(e2)) match {
      case ((l, r), (l2, r2)) => (l ++ l2, r ++ r2)
    }
    case Unary(EXT, ATToken(e)) => (Set(), Set(e))
    case ATToken(e) => (Set(e), Set())
    case _ => (Set(), Set())
  }

  protected def collectTags(e: Expr): (Set[String], Set[String]) = e match {
    case Binary(AND, e1, e2)=> (collectTags(e1), collectTags(e2)) match {
      case ((l, r), (l2, r2)) => (l ++ l2, r ++ r2)
    }
    case Unary(EXT, LVar(e)) => (Set(), Set(e))
    case LVar(e) => (Set(e), Set())
    case _ => (Set(), Set())
  }
}
