package rbparser

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
  protected lazy val t_plus: PackratParser[PLUS] = "+" ^^^ PLUS()
  protected lazy val t_minus: PackratParser[MINUS] = "-" ^^^ MINUS()
  protected lazy val t_ast: PackratParser[AST] = "*" ^^^ AST()
  protected lazy val t_div: PackratParser[DIV] = "/" ^^^ DIV()
  protected lazy val t_and: PackratParser[AND] = "&&" ^^^ AND()
  protected lazy val t_or: PackratParser[OR] = "||"  ^^^ OR()
  protected lazy val t_gt: PackratParser[GT] = ">" ^^^ GT()
  protected lazy val t_ge: PackratParser[GE] = ">=" ^^^ GE()
  protected lazy val t_lt: PackratParser[LT] = "<" ^^^ LT()
  protected lazy val t_le: PackratParser[LE] = "<=" ^^^ LE()
  protected lazy val operator: PackratParser[Op] = t_plus | t_minus | t_ast | t_div | t_and | t_or | t_ge | t_gt | t_le | t_lt

  protected lazy val reserved = K_CLS | K_DEF | K_END | K_IF | K_THEN | K_ELSE | K_TRUE | K_FALSE | K_DO | K_RETURN | K_MODULE
  protected lazy val int: PackratParser[IntLit] = T_INT ^^ { case e => IntLit(e.toInt) }
  protected lazy val double: PackratParser[DoubleLit] = T_DOUBLE ^^ { case e => DoubleLit(e.toDouble) }
  protected lazy val string: PackratParser[StringLit] = T_STRING ^^ StringLit
  // `not` method reuturns `()` if succes, this term does not consume tokens
  protected lazy val lvar: PackratParser[LVar] = not(reserved) ~> T_ID ^^ LVar
  protected lazy val ivar: PackratParser[IVar] = T_AT ~> T_SYMBOL ^^ IVar
  protected lazy val const: PackratParser[ConstLit] = T_CONSTANT ^^ ConstLit
  protected lazy val bool: PackratParser[BoolLit] = T_TRUE ^^ { case _ => BoolLit(true) } | T_FALSE ^^ { case _ => BoolLit(false) }
  protected lazy val methName: PackratParser[MethodName] = T_MNAME ^^ MethodName
  protected lazy val varLit: PackratParser[Literal] = ivar | lvar
  protected lazy val symbol: PackratParser[SymbolLit] = T_COLON ~> T_SYMBOL ^^ SymbolLit

  protected lazy val valWithNot: PackratParser[Unary] = (T_EX ~ (bool | const | lvar | ivar | expr | (T_LPAREN ~> expr <~ T_RPAREN))) ^^ { case op ~ v => Unary(EXT(), v) }
  protected lazy val valMinus: PackratParser[Unary] = (t_minus ~ (const | lvar | ivar | double | int | (T_LPAREN ~> expr <~ T_RPAREN))) ^^ { case op ~ v => Unary(op, v) }
  protected lazy val literal: PackratParser[Expr] = symbol | double | int
  protected lazy val ret: PackratParser[Expr] = T_RETURN ~> aArgs.? ^^ { case a => Return(a.getOrElse(Nil)) }
  protected lazy val aref: PackratParser[ARef] = (primary <~ T_LB) ~ (primary <~ T_RB) ^^ { case v ~ ref => ARef(v, ref) }

  //add inheritace
  protected lazy val classExpr: PackratParser[ClassExpr] = (T_CLS ~> const) ~ (stmnts <~ T_END) ^^ { case name ~ body => ClassExpr(name, body) }
  protected lazy val moduleExpr: PackratParser[ClassExpr] = (T_MODULE ~> const) ~ (stmnts <~ T_END) ^^ { case name ~ body => ClassExpr(name, body) }

  protected lazy val fArgs: PackratParser[List[LVar]] = lvar ~ (T_COMMA ~> fArgs).* ^^ {
    case v ~ Nil => List(v)
    case v ~ List(ids) => v :: ids
  }

  protected lazy val fomalArgs: PackratParser[FormalArgs] =  T_LPAREN ~> fArgs.? <~ T_RPAREN ^^ {
    case args => FormalArgs(args.getOrElse(Nil))
  }

  // FIX: expr is too loose, use strict type
  protected lazy val aArgs: PackratParser[List[Expr]] = arg ~ (T_COMMA ~> aArgs).* ^^ {
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

  protected lazy val ifExpr: PackratParser[IfExpr] = (T_IF ~> expr) ~ (stmnts <~ T_END) ^^ { case cond ~ body => IfExpr(cond, body) }
  protected lazy val methodCall: PackratParser[Call] = lvar ~ actualArgs ^^ {
    case LVar(name) ~ ActualArgs(Nil) => Call(None, MethodName(name), None)
    case LVar(name) ~ args => Call(None, MethodName(name), Some(args))
  } |
  (primary <~ T_DOT) ~ methName ~ actualArgs ^^ {
    case recv ~ name ~ ActualArgs(Nil) => Call(Some(recv), name, None)
    case recv ~ name ~ args => Call(Some(recv), name, Some(args))
  }

  protected lazy val commandArgs: PackratParser[ActualArgs] = aArgs.? ^^ {
    case args => ActualArgs(args.getOrElse(Nil))
  }

  //TODO add COLON call e.g. a::b
  protected lazy val command: PackratParser[Call] = lvar ~ aArgs ^^ {
    // case LVar(name) ~ ActualArgs(Nil) => Call(None, MethodName(name), None)
    // TODO: identifly method call that has no paren and args and varable reference
    case LVar(name) ~ args => Call(None, MethodName(name), Some(ActualArgs(args)))
  } |
  (primary <~ T_DOT) ~ methName ~ commandArgs ^^ {
    case recv ~ name ~ ActualArgs(Nil) => Call(Some(recv), name, None)
    case recv ~ name ~ args => Call(Some(recv), name, Some(args))
  }

  protected lazy val commadCall: PackratParser[Call] = command //  | blockCommand

  protected lazy val exprR: PackratParser[(Op, Expr)] = operator ~ arg ^^ { case op ~ f => (op, f) }

  protected lazy val lhs: PackratParser[Expr] = (primary <~ T_DOT) ~ (methName | const) ^^ {
    case rev ~ ConstLit(c) => Call(Some(rev), MethodName(c), None)
    case rev ~ MethodName(c) => Call(Some(rev), MethodName(c), None)
  }  | primary

  protected lazy val assign: PackratParser[Assign] = lhs ~ (T_OREQ | T_ANDEQ | T_EQ) ~ expr ^^ {
    case name ~ T_EQ ~ value => Assign(name, value)
    case name ~ T_OREQ ~ value => Assign(name, value)
    case name ~ T_ANDEQ ~ value => Assign(name, value)
  }

  protected lazy val postModifier: PackratParser[Expr] = stmnt ~ (T_IF|T_UNLESS) ~ expr ^^ {
    case body ~ T_IF ~ cond => IfExpr(cond, Stmnts(List(body)))
    case body ~ T_UNLESS ~ cond => UnlessExpr(cond, Stmnts(List(body)))
  }

  //TODO ADD `( stamnt  )`
  protected lazy val primary: PackratParser[Expr] =
     valMinus | valWithNot | literal | string | bool | const | symbol | aref | ivar | lvar  | T_LPAREN ~> expr <~ T_RPAREN | ret | methodCall | ifExpr | classExpr | moduleExpr | defExpr

  protected lazy val arg: PackratParser[Expr] = assign | arg ~ exprR.* ^^ { case f ~ e => makeBin(f, e) } | primary
  // protected lazy val arg: PackratParser[Expr] = assign | arg ~ operator ~ arg ^^ { case a ~ o ~ c => Prim(o, a, c) } | primary

  protected lazy val expr: PackratParser[Expr] =  ret | commadCall | arg
  protected lazy val stmnt: PackratParser[Expr] = postModifier | assign | expr
  protected lazy val stmnts: PackratParser[Stmnts] = (stmnt <~ (EOL | T_SCOLON)).* ^^ { case e => Stmnts(e) }

  protected def makeBin(lh: Expr, rh: List[(Op, Expr)]): Expr = {
    innerMakeBin(lh, rh, 0) match {
      case (Nil, expr) => expr
      case (e, expr) => throw new Exception(e.toString())
    }
  }

  private def innerMakeBin(factor: Expr, input: List[(Op, Expr)], prec: Int): (List[(Op, Expr)], Expr) = input match {
    case Nil => (Nil, factor)
    case (op, rhs) :: xs if op.prec < prec => (Nil, factor)
    case (op, rhs) :: xs => {
      val (e1, rest) = xs.span { case (o, e) => o.prec > op.prec }
      val newRhs = e1.foldLeft(rhs) { case (a, (op, e)) => Binary(op, a, e) }
      innerMakeBin(Binary(op, factor, newRhs), rest, 0)
    }
  }
}
