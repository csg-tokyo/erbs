package rbparser

import scala.util.control.Breaks
import scala.util.parsing.combinator.{RegexParsers, PackratParsers}

class Parser extends RegexParsers with PackratParsers with Tokens {
  def parse(in: String): Either[String, Stmnts] = {
    parseAll(stmnts, in) match {
      case Success(d, next) => Right(d)
      case NoSuccess(errorMsg, next) =>
        Left(s"$errorMsg: in ${next.pos.line} at column ${next.pos.column}")
    }
  }

  protected lazy val EOL = opt('\r') <~ '\n'
  protected lazy val reserved = K_CLS | K_DEF | K_END | K_IF | K_THEN | K_ELSE | K_TRUE | K_FALSE | K_DO
  protected lazy val int: PackratParser[IntLit] = T_INT ^^ { case e => IntLit(e.toInt) }
  protected lazy val double: PackratParser[DoubleLit] = T_DOUBLE ^^ { case e => DoubleLit(e.toDouble) }
  protected lazy val string: PackratParser[StringLit] = T_STRING ^^ StringLit
  // `not` method reuturns `()` if succes, this term does not consume tokens
  protected lazy val id: PackratParser[IdLit] = not(reserved) ~> T_ID ^^ IdLit
  protected lazy val instanceVar: PackratParser[InstVarLit] = T_INSTANCE_VAR ^^ { case e => InstVarLit(e.drop(1)) }
  protected lazy val const: PackratParser[ConstLit] = T_CONSTANT ^^ ConstLit
  protected lazy val bool: PackratParser[BoolLit] = T_TRUE ^^ { case _ => BoolLit(true) } | T_FALSE ^^ { case _ => BoolLit(false) }
  protected lazy val methName: PackratParser[MethodName] = T_MNAME ^^ MethodName
  protected lazy val varLit: PackratParser[Var] = instanceVar | id

  protected lazy val classExpr: PackratParser[ClassExpr] = (T_CLS ~> const) ~ (stmnts <~ T_END) ^^ { case name ~ body => ClassExpr(name, body) }
  protected lazy val valWithNot: PackratParser[Unary] = (T_EX ~ (bool | const | id | instanceVar | expr | (T_LPAREN ~> expr <~ T_RPAREN))) ^^ { case op ~ v => Unary(EXT(), v) }
  protected lazy val valMinus: PackratParser[Unary] = (T_MINS ~ (const | id | instanceVar | double | int | (T_LPAREN ~> expr <~ T_RPAREN))) ^^ { case op ~ v => Unary(MINUS(), v) }

  protected lazy val fArgs: PackratParser[List[IdLit]] = id ~ (T_COMMA ~> fArgs).* ^^ {
    case v ~ Nil => List(v)
    case v ~ List(ids) => v :: ids
  }

  protected lazy val fomalArgs: PackratParser[FormalArgs] =  T_LPAREN ~> fArgs.? <~ T_RPAREN ^^ {
    case args => FormalArgs(args.getOrElse(Nil))
  }

  // FIX: expr is too loose, use strict type
  protected lazy val aArgs: PackratParser[List[Expr]] = expr ~ (T_COMMA ~> aArgs).* ^^ {
    case v ~ Nil => List(v)
    case v ~ List(ids) => v :: ids
  }

  protected lazy val actualArgs: PackratParser[ActualArgs] =  T_LPAREN ~> aArgs.? <~ T_RPAREN ^^ {
    case args => ActualArgs(args.getOrElse(Nil))
  }

  protected lazy val defExpr: PackratParser[DefExpr] = (T_DEF ~> methName) ~ fomalArgs.? ~ (stmnts <~ T_END) ^^ {
    case name ~ (None | Some(FormalArgs(Nil))) ~ body => DefExpr(name, None, body)
    case name ~ args ~ body => DefExpr(name, args, body)
  }

  protected lazy val assign: PackratParser[Assign] = (varLit <~ T_EQ) ~ expr ^^ { case name ~ value => Assign(name, value) }

  protected lazy val fact: PackratParser[Expr] = mCall | valMinus | valWithNot | instanceVar | string | bool | double | int | id | const | T_LPAREN ~> expr <~ T_RPAREN
  protected lazy val operator: PackratParser[Op] = (T_ORE | T_OR | T_AND | T_PLUS | T_MINS | T_AST | T_DIV | T_GE | T_GT | T_LE | T_LT) ^^ {
    case "+" => PLUS()
    case "-" => MINUS()
    case "*" => AST()
    case "/" => DIV()
    case ">" => GT()
    case ">=" => GE()
    case "<" => LT()
    case "<=" => LE()
    case "||" => OR()
    case "&&" => AND()
    case "||=" => ORE()
  }

  protected lazy val mCall: PackratParser[MCall] = fact ~ (T_DOT ~> methName) ~ actualArgs.? ^^ {
    case v ~ id ~ (None | Some(ActualArgs(Nil))) => MCall(v, id, None)
    case v ~ id ~ args => MCall(v, id, args)
  }

  protected lazy val exprR: PackratParser[(Op, Expr)] = operator ~ fact ^^ { case op ~ f => (op, f) }

  protected lazy val expr: PackratParser[Expr] = assign | fact ~ exprR.* ^^ { case f ~ e => makePrim(f, e) }

  protected lazy val stmnt: PackratParser[Stmnt] = defExpr | classExpr | expr

  protected lazy val stmnts: PackratParser[Stmnts] = (stmnt <~ (EOL | T_SCOLON)).* ^^ { case e => Stmnts(e) }

  protected def makePrim(lh: Expr, rh: List[(Op, Expr)]): Expr = {
    innerMakePrim(lh, rh, 0) match {
      case (Nil, expr) => expr
      case (e, expr) => throw new Exception(e.toString())
    }
  }

  private def innerMakePrim(factor: Expr, input: List[(Op, Expr)], prec: Int): (List[(Op, Expr)],Expr) = input match {
    case Nil => (Nil, factor)
    case (op, rhs) :: Nil => (Nil, Prim(op, factor, rhs))
    case (op, rhs) :: (x@((op2, rhs2) :: _)) => {
      if (prec > op.prec) { return (Nil, factor) }
      val a = x.takeWhile { case (o, e) => o.prec > op.prec }
      val b = x.dropWhile { case (o, e) => o.prec > op.prec }
      val c = a.foldLeft(rhs) { case (a, (op, e)) => Prim(op, a, e) }
      innerMakePrim(Prim(op, factor, c), b, 0)
    }
  }
}
