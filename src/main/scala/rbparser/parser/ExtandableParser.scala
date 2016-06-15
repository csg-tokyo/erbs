package rbparser
package parser
import scala.collection.mutable.{Map => MMap}

class ExtendableParser extends RubyParser with OperatorToken {
  protected val DEFAULT_TAG = "origin"

  // terminal -> MatchedAst
  protected val pmap: ParserMap[String, Expr] = new ParserMap[String, Expr]()

  override def stmnts: PackratParser[Stmnts] = ((defop | stmnt) <~ (EOL | ";")).* ^^ Stmnts
  override def reserved = K_OPERATOR | K_WHERE | super.reserved
  // Parse each item of syntax interleaved by '\s'
  protected lazy val v: PackratParser[String] = """[^}\s]+""".r ^^ identity

  protected lazy val tagBinary: PackratParser[Expr] = tagExpr ~ tagExprR.* ^^ { case f ~ e => DNFBuilder.build(makeBin(f, e)) }
  protected lazy val tagExprR: PackratParser[(Op, Expr)] = tagOp ~ tagExpr ^^ { case op ~ f => (op, f) }
  protected lazy val tagOp: PackratParser[Op] = t_and | t_or
  protected lazy val tagExLVar: PackratParser[Expr] = "!" ~> lvar ^^ { Unary(EXT(), _) }
  protected lazy val tagExpr: PackratParser[Expr] = tagExLVar | lvar  | "(" ~> tagBinary <~ ")"

  protected lazy val tagSymbolKey: PackratParser[String] = lvar <~ ":" ^^ { case LVar(v) => v }
  protected lazy val tagKeyValue: PackratParser[Map[String, Expr]] = tagSymbolKey ~ tagBinary ^^ { case k ~ v => Map(k -> v) }
  protected lazy val tagHashBody: PackratParser[Map[String, Expr]] = rep1sep(tagKeyValue, ",") ^^ { _.reduceLeft { (acc, e) => acc ++ e } }
  protected lazy val tagHash: PackratParser[Map[String, Expr]] = "{" ~>  tagHashBody.? <~ "}" ^^ { _.getOrElse(Map.empty) }

  protected lazy val opTagPredicate: PackratParser[Map[String, Expr]] = "where" ~> tagHash
  protected lazy val opSyntax: PackratParser[Syntax] = ("{" ~> v.+ <~ "}") ~ opTagPredicate.? ^^ {
    case list ~ tags => Syntax(tags.getOrElse(Map.empty), list)
  }
  protected lazy val opSemantics: PackratParser[Expr] = "{" ~> stmnt <~ "}"
  protected lazy val opTags: PackratParser[Set[String]] = formalArgs ^^ { case FormalArgs(args) => args.map { case LVar(v) => v}.toSet }
  protected lazy val opDefinition: PackratParser[(Syntax, Expr)] = opSyntax ~ ("=>" ~> opSemantics) ^^ { case syntax ~ body => (syntax, body) }
  protected lazy val defop: PackratParser[Operators] = "operator_with" ~> opTags ~ opDefinition.+ <~ "end" ^^ {
    case tags ~ definitions =>
      val ops = Operators( definitions.map { case (syntax, body) => Operator(tags, syntax, body) } )
      extendWith(ops)
      ops
  }

  protected def extendWith(ops: Operators): Unit = ops match { case Operators(x) => x.foreach { x: Operator => extendWith(x) } }

  protected def extendWith(op: Operator): Unit = {
    val tags = op.tags
    val newParser = buildParser(op)
    pmap.put(tags, newParser)

    // Should extend default no terminal class stmnt
    if (tags.contains(DEFAULT_TAG)) {
      val tmp = stmnt
      stmnt = newParser | tmp
    }
  }

  protected def findParser(cond: Expr): Option[PackratParser[Expr]] = cond match {
    case Binary(OR(), l, r) => for (e1 <- findParser(l); e2 <- findParser(r)) yield { e1 | e2 }
    case e@Binary(AND(), _, _) => collectTags(e) match {
      case List(DEFAULT_TAG) => Some(stmnt)
      case x => pmap.getWithAllMatch(x)
    }
    case LVar(key) => if (DEFAULT_TAG == key) Some(stmnt) else pmap.get(key)
    case x => println(x); throw new Exception // TODO FIX
  }

  protected def collectTags(e: Expr): Set[String] = e match {
    case Binary(AND(), e1, e2)=> collectTags(e1) ++ collectTags(e2)
    case LVar(e) => Set(e)
    case _ => throw new Exception()
  }

  protected def buildParser(op: Operator): PackratParser[Expr] = {
    val parsers = op.syntax.body.map { term =>
      op.syntax.tags.get(term) match {
        case None => val v: PackratParser[Map[String, Expr]] = term ^^^ Map.empty[String, Expr]; v
        case Some(cond) => findParser(cond).get ^^ { case ast => Map(term -> ast) } // TOFIX raise execption
      }
    }

    parsers.reduceLeft { (acc, v) => acc ~ v ^^ { case m1 ~ m2 => m1 ++ m2 } } ^^ {
      case map => MacroConverter.convert(op.body, map)
    }
  }

  object ParserMap {
    def empty[T, S] = new ParserMap[T, S]()
  }

  class ParserMap[T, S] (m: MMap[Set[T], PackratParser[S]] = MMap.empty[Set[T], PackratParser[S]]) {
    def get(k: T) = searchBy(_.contains(k))


    def getWithAllMatch(k: Set[T]) = searchBy(k.subsetOf(_))

    def put(key: Set[T], value: PackratParser[S]) = m.get(key) match {
      case None => m.put(key, value)
      case Some(parser) => m.put(key, value | parser)
    }

    private def searchBy(cond: Set[T] => Boolean): Option[PackratParser[S]] =
      m.filterKeys(cond).values.reduceLeftOption { (acc, v) => acc | v }
  }
}

trait OperatorToken {
  val K_OPERATOR =  """operator_with\b""".r
  val K_WHERE =  """where\b""".r
}
