package rbparser
package parser
import scala.collection.mutable.{Map => MMap}

class ExtendableParser extends RubyParser with OperatorToken {
  val DEFAULT_TAG = "origin"

  // x => originParser
  protected var pmap: MMap[String, PackratParser[Expr]] = MMap.empty[String, PackratParser[Expr]]

  override def stmnts: PackratParser[Stmnts] = ((defop | stmnt) <~ (EOL | T_SCOLON)).* ^^ Stmnts
  override def reserved = K_OPERATOR | super.reserved
  // Parse each item of syntax
  protected lazy val v: PackratParser[String] = """[^}\s]+""".r ^^ identity

  protected lazy val tagBinary: PackratParser[Expr] = tagExpr ~ tagExprR.* ^^ { case f ~ e => makeBin(f, e) }
  protected lazy val tagExprR: PackratParser[(Op, Expr)] = tagOp ~ tagExpr ^^ { case op ~ f => (op, f) }
  protected lazy val tagOp: PackratParser[Op] = t_and | t_or
  protected lazy val tagExpr: PackratParser[Expr] = "!".? ~ lvar ^^ {
    case Some(_) ~ v => Unary(EXT(), v)
    case None ~ v => v
  } | "(" ~> tagBinary <~ ")"

  protected lazy val tagHash: PackratParser[Map[String, Expr]] = "{" ~>  tagHashBody.? <~ "}" ^^ { case args => args.getOrElse(Map.empty) }
  protected lazy val tagHashBody: PackratParser[Map[String, Expr]] = tagKeyValues <~ T_COMMA.?
  protected lazy val tagSymbolKey: PackratParser[LVar] = lvar <~ T_COLON
  protected lazy val tagKeyValues: PackratParser[Map[String, Expr]] = tagSymbolKey ~ tagBinary ~ (T_COMMA ~> tagKeyValues).* ^^ {
    case LVar(k) ~ v ~ Nil => Map(k -> v)
    case LVar(k) ~ v ~ List(m) => Map(k -> v) ++ m
  }

  protected lazy val opTagPredicate: PackratParser[Map[String, Expr]] = T_WHERE ~> tagHash
  protected lazy val opSyntax: PackratParser[Syntax] = ("{" ~> v.+ <~ "}") ~ opTagPredicate ^^ {
    case list ~ tags => Syntax(tags, list)
  }
  protected lazy val opSemantics: PackratParser[Expr] = "{" ~> stmnt <~ "}"
  protected lazy val opTags: PackratParser[List[String]] = formalArgs ^^ { case FormalArgs(args) => args.map { case LVar(v) => v} }
  protected lazy val defop: PackratParser[Operators] = (T_OPERATOR ~> opTags) ~ (opDefinition.+ <~ "end") ^^ {
    case tags ~ definitions =>
      val ops = Operators( definitions.map { case (syntax, body) => Operator(tags, syntax, body) } )
      extendWith(ops)
      ops
  }

  protected lazy val opDefinition: PackratParser[(Syntax, Expr)] = opSyntax ~ ("=>" ~> opSemantics) ^^ { case syntax ~ body => (syntax, body) }

  protected def extendWith(ops: Operators): Unit = ops match { case Operators(x) =>  x.foreach { x: Operator => extendWith(x) } }

  protected def extendWith(op: Operator): Unit = {
    val p = buildParser(op) ^^ { case map => MacroConverter.convert(op.body, map) }
    op.tags.foreach { tag =>
      pmap.get(tag) match {
        case None => pmap.put(tag, p)
        case Some(pp) =>
          if (tag != DEFAULT_TAG) pmap.put(tag, p | pp)
          else {
            val tmp = stmnt
            stmnt = p | tmp
          }
      }
    }
  }

  protected def findOrCreateParser(cond: Expr): PackratParser[Expr] = cond match {
    case LVar(key) => pmap.get(key).getOrElse {
      val base = if (key == DEFAULT_TAG) this else new ExtendableParser
      val stmt = base.stmnt.asInstanceOf[PackratParser[Expr]]
      pmap.put(key,  stmt)
      stmt
    }
    case Binary(OR(), e1, e2)=> findOrCreateParser(e1) | findOrCreateParser(e2)
    case Binary(AND(), e1, e2)=> findOrCreateParser(e1) | findOrCreateParser(e2)
    case x => println(x); throw new Exception
  }

  protected def buildParser(op: Operator): PackratParser[Map[String, Expr]] = {
    val parsers = op.syntax.body.map { term =>
      op.syntax.tags.get(term) match {
        case None => val v: PackratParser[Map[String, Expr]] = term ^^^ Map.empty[String, Expr]; v
        case Some(cond) => findOrCreateParser(cond) ^^ { case x => Map(term -> x) }
      }
    }
    parsers.tail.foldLeft(parsers.head) {
      case (acc, v) => acc ~ v ^^ { case m1 ~ m2 => m1 ++ m2 }
    }
  }

  object ParserMap {
    def empty[T, S] = new ParserMap[T, S]()
  }

  class ParserMap[T, S] (m: MMap[List[T], PackratParser[S]] = MMap.empty[List[T], PackratParser[S]]) {
    def get(k: T) = searchBy(_.contains(k))

    def getWithAllMatch(k: List[T]) = searchBy { key => k.forall(key.contains(_)) }

    def getWithSomeMatch(k: List[T]) = searchBy { key => k.exists(key.contains(_)) }

    def put(key: List[T], value: PackratParser[S]) = m.get(key) match {
      case None => m.put(key, value)
      case Some(parser) => m.put(key, value | parser)
    }

    def searchBy(cond: List[T] => Boolean): Option[PackratParser[S]] =
      m.keys.toList.filter(cond).map(m(_)).reduceLeftOption { (acc, v) => acc | v }
  }
}

trait OperatorToken {
  val T_OPERATOR = "operator_with"
  val K_OPERATOR =  """operator_with\b""".r

  val T_WHERE = "where"
  val K_WHERE =  """where\b""".r
}
