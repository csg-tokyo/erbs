package rbparser
package parser

import util._
import ast._
import token.OperatorToken
import scala.collection.mutable.{Map => MMap}

class ExtendableParser extends RubyParser with OperatorToken with ParserMap {
  protected val DEFAULT_TAG = "origin"

  // terminal -> MatchedAst
  protected val pmap: PMap[String, Expr] = PMap.empty[String, Expr]
  protected val omap: MMap[String, List[Operator]] =  MMap.empty[String, List[Operator]]

  override def parse(in: String): Either[String, Stmnts] = {
    parseAll(topStmnts, preprocess(in)) match {
      case Success(d, next) => Right(d)
      case NoSuccess(errorMsg, next) =>
        Left(s"$errorMsg: in ${next.pos.line} at column ${next.pos.column}")
    }
  }

  // Insert operator
  def topStmnts: PackratParser[Stmnts] = stmnts.map {
    case Stmnts(x) =>
      val body = omap.toList.map { case (k, v) => ClassExpr(ConstLit(k), Stmnts(v.map(_.toMethod))) }
      Stmnts(if (body.size == 0) x else ModuleExpr(ConstLit("Operator"), Stmnts(body)) :: x)
  }

  override def stmnts: PackratParser[Stmnts] = ((defop | stmnt) <~ (EOL | ";")).* ^^ Stmnts
  override def reserved = K_OPERATOR | K_DEFS | super.reserved
  // Parse each item of syntax interleaved by '\s'
  protected lazy val v: PackratParser[String] = """[^()\s]+""".r ^^ identity

  protected lazy val tagBinary: PackratParser[Expr] = tagExpr ~ tagExprR.* ^^ { case f ~ e => DNFBuilder.build(makeBin(f, e)) }
  protected lazy val tagExprR: PackratParser[(Op, Expr)] = tagOp ~ tagExpr ^^ { case op ~ f => (op, f) }
  protected lazy val tagOp: PackratParser[Op] = t_and | t_or
  protected lazy val tagExLVar: PackratParser[Expr] = "!" ~> lvar ^^ { Unary(EXT(), _) }
  protected lazy val tagExpr: PackratParser[Expr] = tagExLVar | lvar  | "(" ~> tagBinary <~ ")"

  protected lazy val tagSymbolKey: PackratParser[String] = lvar <~ ":" ^^ { case LVar(v) => v }
  protected lazy val tagKeyValue: PackratParser[Map[String, Expr]] = tagSymbolKey ~ tagBinary ^^ { case k ~ v => Map(k -> v) }
  protected lazy val tagHashBody: PackratParser[Map[String, Expr]] = rep1sep(tagKeyValue, ",") ^^ { _.reduceLeft { (acc, e) => acc ++ e } }

  protected lazy val opTagPredicate: PackratParser[Map[String, Expr]] = "(" ~> tagHashBody.? <~ ")" ^^ { _.getOrElse(Map.empty) }
  protected lazy val opSyntax: PackratParser[Syntax] =  v.+ ~ opTagPredicate.? ^^ {
    case list ~ tags => Syntax(tags.getOrElse(Map.empty), list)
  }
  protected lazy val opSemantics: PackratParser[Expr] = stmnt
  protected lazy val opTags: PackratParser[Set[String]] = formalArgs ^^ { case FormalArgs(args) => args.map { case LVar(v) => v}.toSet }
  protected lazy val opDefinition: PackratParser[(Syntax, Expr)] = "defs" ~> opSyntax ~ opSemantics <~ "end" ^^ { case syntax ~ body => (syntax, body) }

  protected lazy val defop: PackratParser[Operators] = "Operator" ~> opTags ~ opDefinition.+ <~ "end" ^^ {
    case tags ~ definitions =>
      val ops = Operators( definitions.map { case (syntax, body) => Operator(tags, syntax, body) } )
      extendWith(ops)
      ops
  }

  protected def extendWith(ops: Operators): Unit = ops match { case Operators(x) => x.foreach { x: Operator => extendWith(x) } }

  protected def extendWith(op: Operator): Unit = {
    val tags = op.tags
    register_operator(op)
    val newParser = buildParser(op)
    pmap.put(tags, newParser)

    // Should extend default no terminal class stmnt
    if (tags.contains(DEFAULT_TAG)) {
      val tmp = stmnt
      stmnt = newParser | tmp
    }
  }

  class NoSuchTags(message :String = null, cause :Throwable = null) extends RuntimeException(message, cause)
  class NoSuchParser(message :String = null, cause :Throwable = null) extends RuntimeException(message, cause)

  protected def dummyParser(op : Operator): PackratParser[Expr] = stmnt ^^^ {
    throw new NoSuchTags(s"Calling dummpy parser in ${op.syntaxBody}")
  }

  protected def findParser(cond: Expr): Option[PackratParser[Expr]] = cond match {
    case Binary(OR(), l, r) => for (e1 <- findParser(l); e2 <- findParser(r)) yield { e1 | e2 }
    case e@Binary(AND(), _, _) => collectTags(e) match {
      case (t, nt) if t == Set(DEFAULT_TAG) && nt == Set() => Some(stmnt)
      case (t, nt) => pmap.getWithAllMatch(t, nt)
    }
    case Unary(EXT(), LVar(e)) => pmap.getNot(e)
    case LVar(key) => if (DEFAULT_TAG == key) Some(stmnt) else pmap.get(key)
    case x => throw new  NoSuchParser(x.toString()) // TODO FIX
  }

  protected def collectTags(e: Expr): (Set[String], Set[String]) = e match {
    case Binary(AND(), e1, e2)=> (collectTags(e1), collectTags(e2)) match {
      case ((l, r), (l2, r2)) => (l ++ l2, r ++ r2)
    }
    case Unary(EXT(), LVar(e)) => (Set(), Set(e))
    case LVar(e) => (Set(e), Set())
    case _ => (Set(), Set())
  }

  protected def register_operator(op: Operator) = omap.get(op.className) match {
    case Some(x) => omap.put(op.className, op :: x)
    case None => omap.put(op.className, List(op))
  }

  protected def buildParser(op: Operator): PackratParser[Expr] = {
    val parsers = op.syntaxBody.map { term =>
      op.syntaxTags.get(term) match {
        case None => val v: PackratParser[Map[String, Expr]] = term ^^^ Map.empty[String, Expr]; v
        case Some(cond) => findParser(cond) match {
          case Some(p) => p ^^ { case ast => Map(term -> ast) }
          case None => throw new NoSuchParser(s"$term (tags of ${PrettyPrinter.call(cond)}) in ${op.syntaxBody}")
        }
      }
    }

    parsers.reduceLeft { (acc, v) => acc ~ v ^^ { case m1 ~ m2 => m1 ++ m2 } } ^^ { op.toMethodCall(_) }
  }
}
