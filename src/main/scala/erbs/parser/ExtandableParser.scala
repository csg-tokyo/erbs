package erbs
package parser

import util._
import ast._
import token.OperatorToken
import scala.collection.mutable.{Map => MMap}

class ExtendableParser extends RubyParser with OperatorToken with MapUtil {
  private class NoSuchParser(message :String = null, cause :Throwable = null) extends RuntimeException(message, cause)
  private class InvalidCondition(message :String = null, cause :Throwable = null) extends RuntimeException(message, cause)

  protected val DEFAULT_TAG = "origin"
  protected val pmap: ParserMap[String, Expr] = ParserMap.empty[String, Expr]
  protected val omap: OperatorMap =  OperatorMap.empty

  override def stmnts: Parser[Stmnts] = midStatmnts ^^ { _.prependExpr(omap.toModule) }
  protected def midStatmnts: Parser[Stmnts] = ((defop | stmnt) <~ (EOL | ";")).* ^^ Stmnts
  override def reserved = K_OPERATOR | K_DEFS | super.reserved
  // Parse each item of syntax interleaved by '\s'
  protected lazy val v: PackratParser[String] = """[^()\s]+""".r ^^ identity

  protected lazy val tagBinary: PackratParser[Expr] = tagExpr ~ tagExprR.* ^^ { case f ~ e => DNFBuilder.build(makeBin(f, e)) }
  protected lazy val tagExprR: PackratParser[(Op, Expr)] = tagOp ~ tagExpr ^^ { case op ~ f => (op, f) }
  protected lazy val tagOp: PackratParser[Op] = t_and | t_or
  protected lazy val tagExLVar: PackratParser[Expr] = "!" ~> lvar ^^ { Unary(EXT, _) }
  protected lazy val tagExpr: PackratParser[Expr] = tagExLVar | lvar  | "(" ~> tagBinary <~ ")"

  protected lazy val tagSymbolKey: PackratParser[String] = lvar <~ ":" ^^ { case LVar(v) => v }
  protected lazy val tagKeyValue: PackratParser[Map[String, Expr]] = tagSymbolKey ~ tagBinary ^^ { case k ~ v => Map(k -> v) }
  protected lazy val tagHashBody: PackratParser[Map[String, Expr]] = rep1sep(tagKeyValue, ",") ^^ { _.reduceLeft { (acc, e) => acc ++ e } }

  protected lazy val opTagPredicate: PackratParser[Map[String, Expr]] = "(" ~> tagHashBody.? <~ ")" ^^ { _.getOrElse(Map.empty) }
  protected lazy val opSyntax: PackratParser[Syntax] =  v.+ ~ opTagPredicate.? ^^ { case list ~ tags => Syntax(tags.getOrElse(Map.empty), list) }
  protected lazy val opSemantics: PackratParser[Stmnts] = midStatmnts
  protected lazy val opTags: PackratParser[Set[String]] = formalArgs ^^ { case FormalArgs(args) => args.map { case LVar(v) => v}.toSet }
  protected lazy val opDefinition: PackratParser[(Syntax, Stmnts)] = "defs" ~> opSyntax ~ opSemantics <~ "end" ^^ { case syntax ~ body => (syntax, body) }

  protected lazy val defop: PackratParser[Operators] = "Operator" ~> opTags ~ opDefinition.+ <~ "end" ^^ { case tags ~ definitions =>
    val ops = Operators( definitions.map { case (syntax, body) => Operator(tags, syntax, body) } )
    extendWith(ops)
    ops
  }

  protected def extendWith(operators: Operators): Unit = {
    registerOperators(operators)
    for (op <- operators.ops if op.tags.contains(DEFAULT_TAG)) {
      pmap.getWithAllMatch(op.tags).foreach { p =>
        val tmp = stmnt
        stmnt = p | tmp
      }
    }
  }

  protected def registerOperators(ops: Operators) = ops.foreach { op =>
    omap.put(op)
    pmap.put(op.tags, buildParser(op))
  }

  protected def buildParser(op: Operator): PackratParser[Expr] = opToParsers(op).reduceLeft {
    (acc, v) => acc ~ v ^^ { case m1 ~ m2 => m1 ++ m2 }
  } ^^ op.toMethodCall

  protected def opToParsers(op : Operator): List[Parser[Map[String,Expr]]] = op.syntaxBody.map { term =>
    op.syntaxTags.get(term) match {
      case None => term ^^^ Map.empty[String, Expr]
      case Some(cond) => findParser(cond) match {
        case None => throw new NoSuchParser(s"$term (tags of ${PrettyPrinter.call(cond)}) in ${op.syntaxBody}")
        case Some(p) => p ^^ { ast => Map(term -> ast) }
      }
    }
  }

  protected def findParser(cond: Expr): Option[PackratParser[Expr]] = cond match {
    case Binary(OR, l, r) => for (e1 <- findParser(l); e2 <- findParser(r)) yield { e1 | e2 }
    case e@Binary(AND, _, _) => collectTags(e) match {
      case (t, nt) if t == Set(DEFAULT_TAG) && nt == Set() => Some(stmnt)
      case (t, nt) => pmap.getWithAllMatch(t, nt)
    }
    case Unary(EXT, LVar(e)) => pmap.getNot(e)
    case LVar(key) => if (DEFAULT_TAG == key) Some(stmnt) else pmap.get(key)
    case x => throw new InvalidCondition(x.toString())
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
