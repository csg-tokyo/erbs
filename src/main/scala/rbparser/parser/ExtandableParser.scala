package rbparser
package parser
import scala.collection.mutable.{Map => MMap}

class ExtendableParser extends RubyParser with OperatorToken {
  val DEFAULT_TAG = "origin"

  protected var pmap: MMap[String, PackratParser[Map[String, Expr]]] = MMap(DEFAULT_TAG ->  (stmnt ^^^ Map.empty[String, Expr]))

  override def stmnts: PackratParser[Stmnts] = ((defop | stmnt) <~ (EOL | T_SCOLON)).* ^^ Stmnts
  override def reserved = K_OPERATOR | super.reserved
  // Parse each item of syntax
  protected lazy val v: PackratParser[String] = """[^}\s]+""".r ^^ identity

  protected lazy val tagHash: PackratParser[Map[String, Expr]] = "{" ~>  tagHashBody.? <~ "}" ^^ { case args => args.getOrElse(Map.empty) }
  protected lazy val tagHashBody: PackratParser[Map[String, Expr]] = tagKeyValues <~ T_COMMA.?
  protected lazy val tagKeyValues: PackratParser[Map[String, Expr]] = tagSymbolKey ~ binary ~ (T_COMMA ~> tagKeyValues).* ^^ {
    case LVar(k) ~ v ~ Nil => Map(k -> v)
    case LVar(k) ~ v ~ List(m) => Map(k -> v) ++ m
  }
  protected lazy val tagSymbolKey: PackratParser[LVar] = lvar <~ T_COLON

  // TODO fix, ary and hash are tmporary
  protected lazy val opTagPredicate: PackratParser[Map[String, Expr]] = T_WHERE ~> tagHash
  protected lazy val opSyntax: PackratParser[Syntax] = ("{" ~> v.+ <~ "}") ~ opTagPredicate ^^ {
    case list ~ tags => Syntax(tags, list)
  }
  protected lazy val opSemantics: PackratParser[Expr] = "{" ~> stmnt <~ "}"
  protected lazy val opTags: PackratParser[List[String]] = formalArgs ^^ {
    case FormalArgs(args) => args.map { case LVar(v) => v}
  }

  protected lazy val defop: PackratParser[Operator] = (T_OPERATOR ~> opTags) ~ opSyntax ~ ("=>" ~> opSemantics <~ "end") ^^ {
    case tags ~ syntax ~ body =>
      val op = Operator(tags, syntax, body);
      extendWith(op)
      op
  }

  // protected def extendWith(op:  Operator): PackratParser[Expr] = {
  protected def extendWith(op:  Operator)= {
    val p = buildParser(op)
    op.tags.foreach { x =>
      pmap.get(x) match {
        case None => pmap.put(x, p)
        case Some(pp) =>
          if (x != DEFAULT_TAG) pmap.put(x, p | pp)
          else {
            val tmp = stmnt
            stmnt = p ^^ { case map => MacroConverter.convert(op.body, map) } | tmp
          }
      }
    }
  }

  protected def findOrCreateParser(key: String): PackratParser[Map[String, Expr]] = pmap.get(key).getOrElse {
    val ep = new ExtendableParser
    val stmt = (ep.stmnt.asInstanceOf[PackratParser[Expr]] ^^ { case x => Map(key -> x) })
    // default parser
    pmap.put(key,  stmt)
    stmt
  }

  protected def buildParser(op: Operator): PackratParser[Map[String, Expr]] = {
    val parsers = op.syntax.body.map { term =>
      op.syntax.tags.get(term) match {
        case None => val v: PackratParser[Map[String, Expr]] = term ^^^ Map.empty[String, Expr]; v
        case Some(tag_cond) => tag_cond match {
          // case x => ADD more case condtion
          case LVar(cond) => findOrCreateParser(cond)
          case x => println(x);throw new Exception
        }
      }
    }
    parsers.tail.foldLeft(parsers.head) {
      case (acc, v) => acc ~ v ^^ { case m1 ~ m2 => m1 ++ m2 }
    }
  }
}

trait OperatorToken {
  val T_OPERATOR = "operator_with"
  val K_OPERATOR =  """operator_with\b""".r

  val T_WHERE = "where"
  val K_WHERE =  """where\b""".r
}
