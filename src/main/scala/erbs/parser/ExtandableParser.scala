package erbs
package parser

import util._
import ast._
import token.OperatorToken

class ExtendableParser extends RubyParser with OperatorToken with MapUtil with ParserErrors {
  protected val DEFAULT_TAG = "origin"
  protected val hmap: HogeMap[Expr] = new HogeMap

  override def stmnts: Parser[Stmnts] = midStatmnts ^^ { _.prependExpr(hmap.toModule) }
  protected def midStatmnts: Parser[Stmnts] = ((defop | stmnt) <~ (EOL | ";")).* ^^ Stmnts
  override def reserved = K_OPERATOR | K_DEFS | K_AT_TOKEN | super.reserved
  // Parse each items of syntax interleaved by '\s'
  protected lazy val v: PackratParser[String] = """[^()\s]+""".r ^^ identity

  protected lazy val tagBinary: PackratParser[Expr] = tagExpr ~ tagExprR.* ^^ { case f ~ e => DNFBuilder.build(makeBin(f, e)) }
  protected lazy val tagExprR: PackratParser[(Op, Expr)] = tagOp ~ tagExpr ^^ { case op ~ f => (op, f) }
  protected lazy val tagOp: PackratParser[Op] = t_and | t_or
  protected lazy val tagLVar: PackratParser[Expr] = "@token(" ~> lvar <~ ")" ^^  (ATToken(_)) | lvar
  protected lazy val tagExLVar: PackratParser[Expr] = "!" ~> tagLVar ^^ (Unary(EXT, _))
  protected lazy val tagExpr: PackratParser[Expr] = tagExLVar | tagLVar  | "(" ~> tagBinary <~ ")"

  protected lazy val tagSymbolKey: PackratParser[String] = lvar <~ ":" ^^ (_.v)
  protected lazy val tagKeyValue: PackratParser[Map[String, Expr]] = tagSymbolKey ~ tagBinary ^^ { case k ~ v => Map(k -> v) }
  protected lazy val tagHashBody: PackratParser[Map[String, Expr]] = rep1sep(tagKeyValue, ",") ^^ { _.reduceLeft { (acc, e) => acc ++ e } }

  protected lazy val opTagsDefinition: PackratParser[Map[String, Expr]] = "(" ~> tagHashBody.? <~ ")" ^^ { _.getOrElse(Map.empty) }
  protected lazy val opSyntax: PackratParser[Syntax] =  v.+ ~ opTagsDefinition.? ^^ { case list ~ tags => Syntax(tags.getOrElse(Map.empty), list) }
  protected lazy val opSemantics: PackratParser[Stmnts] = midStatmnts
  protected lazy val opTags: PackratParser[Set[String]] = formalArgs ^^ { _.args.map(lvar => lvar.v).toSet }
  protected lazy val opDefinition: PackratParser[(Syntax, Stmnts)] = "defs" ~> opSyntax ~ opSemantics <~ "end" ^^ { case syntax ~ body => (syntax, body) }
  protected lazy val defop: PackratParser[Operators] = "Operator" ~> opTags ~ opDefinition.+ <~ "end" ^^ { case tags ~ d => extendSyntax(tags, d) }

  protected def extendSyntax(tags: Set[String], definitions: Seq[(Syntax, Stmnts)]): Operators = {
    val operators = Operators(tags, definitions)

    // register
    for (op <- operators.ops) { hmap.put(op.tags, op, buildParser(op)) }

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

  protected def buildParser(op: Operator): PackratParser[Expr] =
    opToParsers(op, new Context()).reduceLeft { (acc, v) => acc ~ v ^^ { case m1 ~ m2 => m1 ++ m2 } } ^^ op.toMethodCall

  protected def opToParsers(op : Operator, context: Context): List[Parser[Map[String, Expr]]] =
    op.syntaxBody.map { term =>
      op.syntaxTags.get(term) match {
        case None => term ^^^ Map.empty[String, Expr]
        case Some(cond) => findParser(cond, context) match {
          case None => throw new NoSuchParser(s"$term (tags of ${PrettyPrinter.call(cond)}) in ${op.syntaxBody}")
          case Some(p) => p ^^ { ast => Map(term -> ast) }
        }
      }
    }

  object EmptySet {
    def unapply(s: Set[String]): Boolean = s.isEmpty
  }

  protected def findParser(cond: Expr, context: Context): Option[PackratParser[Expr]] =
    context.fold(cond)(Binary(AND, cond, _)) match {
      case Binary(OR, l, r) => for (e1 <- findParser(l, context); e2 <- findParser(r, context)) yield { e1 | e2 }
      case Unary(EXT, LVar(e)) => hmap.getNot(e)
      case LVar(key) => if (DEFAULT_TAG == key) Some(stmnt) else hmap.get(key)
      case ATToken(key) => None // OK?
      case e@Binary(AND, _, _) => collectTags(e) match {
        case (EmptySet(), EmptySet()) => None // OK?
        case (t, nt) if t == Set(DEFAULT_TAG) && nt == Set() => Some(stmnt)
        case (t, nt) => (collectTokens(e), context.isEmpty) match {
          case ((EmptySet(), EmptySet()), true) => hmap.getWithAllMatch(t, nt)
          case (c, true) => { // New tokens appear, so we attach new context
            val newContxt = context.createNewContext(c)
            //TODO cached
            val hs = hmap.getParsers(t, nt).map { _.selectByContext(newContxt) }.filter(!_.isEmpty)
            val v: Iterable[PackratParser[Expr]] = hs.map { h =>
              //rebuild new parsers with token
              val x: PackratParser[Expr] = h.operators.flatMap(opToParsers(_, newContxt)).reduceLeft {
                (acc, v) => acc ~ v ^^ { case m1 ~ m2 => m1 ++ m2 }
              } ^^ h.operators(0).toMethodCall // TODO fix
              x
            }
            v.reduceLeftOption { (acc, v) => acc | v }
          }
          case (c, false) => {
            val hs = hmap.getParsers(t, nt).map { _.selectByContext(context.createNewContext(c)) }.filter(!_.isEmpty)
            hs.flatMap { case Hoge(_, par) => par }.reduceLeftOption { (acc, v) => () => acc() | v() }.map(_())
          }
        }
      }
      case invalid => throw new InvalidCondition(invalid.toString())
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
