package erbs.parser

import token.Tokens
import ast._

trait RubyParser extends BaseParser[Stmnts] with Tokens {

  override protected def commentLiteral = "#"

  protected lazy val EOL = opt('\r') <~ '\n'
  protected lazy val t_plus: PackratParser[Op] = "+" ^^^ PLUS
  protected lazy val t_minus: PackratParser[Op] = "-" ^^^ MINUS
  protected lazy val t_mul: PackratParser[Op] = "*" ^^^ MUL
  protected lazy val t_div: PackratParser[Op] = "/" ^^^ DIV
  protected lazy val t_and: PackratParser[Op] = "&&" ^^^ AND
  protected lazy val t_or: PackratParser[Op] = "||"  ^^^ OR
  protected lazy val t_eeq: PackratParser[Op] = "==" ^^^ EEQ
  protected lazy val t_gt: PackratParser[Op] = ">" ^^^ GT
  protected lazy val t_ge: PackratParser[Op] = ">=" ^^^ GE
  protected lazy val t_lt: PackratParser[Op] = "<" ^^^ LT
  protected lazy val t_le: PackratParser[Op] = "<=" ^^^ LE
  protected lazy val t_eq: PackratParser[Op] = "=" ^^^ EQ
  protected lazy val t_adde: PackratParser[Op] = "+=" ^^^ ADDE
  protected lazy val t_sube: PackratParser[Op] = "-=" ^^^ SUBE
  protected lazy val t_ande: PackratParser[Op] = "&&=" ^^^ ANDE
  protected lazy val t_ore: PackratParser[Op] = "||=" ^^^ ORE
  protected lazy val t_space: PackratParser[String] = customLiteral(" ")

  protected def reserved = reserved_value
  protected lazy val reserved_value = K_CLS | K_DEF | K_END | K_IF | K_THEN | K_ELSE | K_TRUE | K_FALSE | K_DO | K_RETURN | K_MODULE | K_UNLESS | K_ELSIF
  var operator: PackratParser[Op] = t_plus | t_minus | t_mul | t_div | t_and | t_or | t_ge | t_gt | t_le | t_lt | t_eeq
  protected lazy val int: PackratParser[IntLit] = T_INT ^^ { e => IntLit(e.toInt) }
  protected lazy val double: PackratParser[DoubleLit] = T_DOUBLE ^^ { e => DoubleLit(e.toDouble) }
  protected lazy val string: PackratParser[StringLit] = (T_STRING | T_STRING_SINGLE) ^^ StringLit
  // @ref http://www.scala-lang.org/old/node/11315.html
  protected lazy val lvar: PackratParser[LVar] = not(reserved) ~> T_ID ^^ LVar
  protected lazy val ivar: PackratParser[IVar] = "@" ~> T_SYMBOL ^^ IVar
  protected lazy val const: PackratParser[ConstLit] = T_CONSTANT ^^ ConstLit
  protected lazy val falseValue: PackratParser[BoolLit] = "false" ^^^ BoolLit(false)
  protected lazy val trueValue: PackratParser[BoolLit] = "true" ^^^ BoolLit(true)
  protected lazy val bool: PackratParser[BoolLit] = trueValue | falseValue
  protected lazy val symbol: PackratParser[SymbolLit] = ":" ~> T_SYMBOL ^^ SymbolLit
  protected lazy val symbolKey: PackratParser[SymbolLit] = T_SYMBOL <~ ":" ^^ SymbolLit
  protected lazy val valWithNot: PackratParser[Unary] = "!" ~> (bool | const | lvar | ivar | "(" ~> expr <~ ")" | valWithNot) ^^ { Unary(EXT, _) }
  protected lazy val valMinus: PackratParser[Unary] = "-" ~> (const | lvar | ivar | double | int | "(" ~> expr <~ ")") ^^ { Unary(MINUS, _) }
  protected lazy val literal: PackratParser[Expr] = symbol | double | int
  protected lazy val variable: PackratParser[Literal] = lvar | ivar | const
  protected lazy val ret: PackratParser[Expr] = "return" ~> actualArgList.? ^^ { args => Return(args.getOrElse(Nil).map(_.value)) }

  protected lazy val aref: PackratParser[ARef] = primaryForAref ~ (customLiteral("[") ~> primary <~ "]") ^^ { case v ~ ref => ARef(v, ref) }
  protected lazy val arefArgs: PackratParser[List[Expr]] = actualArgList <~ ",".? ^^ { args => args.map(_.value) }
  protected lazy val primaryForAref: PackratParser[Expr] = valMinus | valWithNot | branchExpr | string | variable | "(" ~> expr <~ ")"
  protected lazy val ary: PackratParser[Ary] = "[" ~>  arefArgs.? <~ "]" ^^ { args => Ary(args.getOrElse(Nil)) }

  protected lazy val symbolHashKey: PackratParser[Expr] = (string <~ ":") | symbolKey
  protected lazy val rocketHashKey: PackratParser[Expr] = arg <~ "=>"
  protected lazy val keyValue: PackratParser[Map[Expr, Expr]] = (symbolHashKey | rocketHashKey) ~ arg ^^ { case k ~ v => Map(k -> v) }
  protected lazy val hashBody: PackratParser[Hash] = rep1sep(keyValue, ",") ^^ { args => Hash(args.reduceLeft { (acc, e) => acc ++ e }) }
  protected lazy val hash: PackratParser[Hash] = "{" ~>  hashBody.? <~ "}" ^^ { _.getOrElse(Hash(Map.empty)) }

  protected lazy val classExpr: PackratParser[ClassExpr] = "class" ~> const ~ ("<" ~> expr).? ~ stmnts <~ "end" ^^ { case name ~ parent ~ body => ClassExpr(name, parent, body) }
  protected lazy val moduleExpr: PackratParser[ModuleExpr] = "module" ~> const ~ stmnts <~ "end" ^^ { case name ~ body => ModuleExpr(name, body) }
  protected lazy val defExpr: PackratParser[DefExpr] = "def" ~> T_DEFMNAME ~ formalArgs.? ~ stmnts <~ "end" ^^ { case name ~ args ~ body => DefExpr(name, args, body) }

  protected lazy val actualArgElement: PackratParser[ActualArgElement] = arg ^^ ActualArgElement
  protected lazy val simpleArgElement: PackratParser[SimpleArgElement] = lvar ^^ SimpleArgElement
  protected lazy val defaultArgElement: PackratParser[DefaultArgElement] = lvar ~ ("=" ~> arg) ^^ { case k ~ v => DefaultArgElement(k, v) }
  protected lazy val keywordArgElement: PackratParser[KeywordArgElement] = (T_SYMBOL <~ ":") ~ arg ^^ { case k ~ v => KeywordArgElement(k, v) }

  protected lazy val simpleArgList: PackratParser[List[SimpleArgElement]] = rep1sep(simpleArgElement, ",")
  protected lazy val actualArgList: PackratParser[List[ActualArgElement]] = rep1sep(actualArgElement, ",")
  protected lazy val formalArgList: PackratParser[List[ArgElement]] = rep1sep(defaultArgElement | keywordArgElement | simpleArgElement, ",")

  protected lazy val formalArgs: PackratParser[FormalArgs] =  "(" ~> formalArgList.? <~ ")" ^^ { args => FormalArgs(args.getOrElse(Nil)) }
  protected lazy val actualArgs: PackratParser[ActualArgs] =  "(" ~> actualArgList.? <~ ")" ^^ { args => ActualArgs(args.getOrElse(Nil)) }
  protected lazy val actualArgsforMethodCall: PackratParser[ActualArgs] =  customLiteral("(") ~> actualArgList.? <~ ")" ^^ { args => ActualArgs(args.getOrElse(Nil)) }

  protected lazy val ifExpr: PackratParser[IfExpr] = "if" ~> expr ~ stmnts ~ elseifBodies ~ ("else" ~> stmnts).? <~ "end" ^^ {
    case cond ~ trueBody ~ eb ~ falseBody => IfExpr(cond, trueBody, eb, falseBody)
  }
  protected lazy val unlessExpr: PackratParser[UnlessExpr] = "unless" ~> expr ~ stmnts <~ "end" ^^ { case cond ~ body => UnlessExpr(cond, body, None) }
  protected lazy val elseifBody: PackratParser[ElsifBody] = "elsif" ~> expr ~ stmnts ^^ { case cond ~ body => ElsifBody(cond, body) }
  protected lazy val elseifBodies: PackratParser[List[ElsifBody]] = elseifBody.*
  protected lazy val branchExpr: PackratParser[Expr] = ifExpr | unlessExpr

  protected lazy val blockParamDef: PackratParser[ActualArgs] = "|" ~> simpleArgList <~ "|" ^^ ActualArgs
  protected lazy val doBlock: PackratParser[Block] = "do" ~> blockParamDef.? ~ stmnts <~ "end" ^^ { case params ~ body => DoBlock(params, body) }
  protected lazy val oneLineBraceBlock: PackratParser[Block] = "{" ~> blockParamDef.? ~ stmnt <~ "}" ^^ {
    case params ~ body =>  BraceBlock(params, Stmnts(List(body)))
  }
  protected lazy val multiLineBraceBlock: PackratParser[Block] = "{" ~> blockParamDef.? ~ stmnts <~ "}" ^^ {
    case params ~ body => BraceBlock(params, body)
  }
  protected lazy val braceBlock: PackratParser[Block] = oneLineBraceBlock | multiLineBraceBlock
  protected lazy val block: PackratParser[Block] = braceBlock | doBlock

  protected lazy val reciverMethodCall: PackratParser[Call] = (primary <~ ".") ~ T_MNAME ~ actualArgsforMethodCall ~ block.? ^^ { case recv ~ name ~ args ~ block => Call(Some(recv), name, Some(args), block) }
  protected lazy val simpleMethodCall: PackratParser[Call] = lvar ~ actualArgsforMethodCall  ~ block.? ^^ { case LVar(name) ~ args ~ block => Call(None, name, Some(args), block) }
  protected lazy val methodCall: PackratParser[Call] = simpleMethodCall | reciverMethodCall
  protected lazy val methodCallNot: PackratParser[Unary] = "!" ~> methodCall ^^ { Unary(EXT, _) }

  // Command call
  protected lazy val commandArgs: PackratParser[ActualArgs] = actualArgList ^^ ActualArgs
  // t_space is needed
  protected lazy val simpleCommand: PackratParser[Cmd] = (lvar <~ t_space) ~ actualArgList ~ block.? ^^ {
    case LVar(name) ~ args ~ block => Cmd(None, name, Some(ActualArgs(args)), block)
  }
  // a.call args
  protected lazy val reciverCommand: PackratParser[Cmd] = (primary <~ ".") ~ T_MNAME ~ (t_space ~> commandArgs).? ~ block.? ^^ {
    case recv ~ name ~ args ~ block => Cmd(Some(recv), name, args, block)
  }
  // TODO add COLON call e.g. a::b
  // command must have at least one
  protected lazy val command: PackratParser[Cmd] = simpleCommand | reciverCommand

  protected lazy val commadCall: PackratParser[Expr] = T_MNAME ~ block ^^ {
    case name ~ block => Cmd(None, name, None, Some(block))
  } | command
  protected lazy val CommadCallNot: PackratParser[Unary] = "!" ~> commadCall ^^ { Unary(EXT, _) }

  protected lazy val exprR: PackratParser[(Op, Expr)] = operator ~ primary ^^ { case op ~ f => (op, f) }

  protected lazy val lhs: PackratParser[Expr] = aref | (primary <~ ".") ~ (T_MNAME | const) ^^ {
    case rev ~ ConstLit(c) => Cmd(Some(rev), c, None, None)
    case rev ~ name => Cmd(Some(rev), name.toString, None, None)
  }  | variable

  // Ignore double assign
  protected lazy val assign: PackratParser[Assign] = lhs ~ (t_eq | t_adde | t_sube | t_ande | t_ore) ~ expr ^^ {
    case name ~ op ~ value => Assign(name, value, op)
  }

  protected lazy val ifPredicate: PackratParser[IfModExpr] = expr ~ ("if" ~> expr) ^^ { case body ~ cond => IfModExpr(cond, body) }
  protected lazy val unlessPredicate: PackratParser[UnlessModExpr] = expr ~ ("unless" ~> expr) ^^ { case body ~ cond => UnlessModExpr(cond, body) }
  protected lazy val postModifier: PackratParser[Expr] = ifPredicate | unlessPredicate

  protected lazy val binary: PackratParser[Expr] = arg ~ exprR.* ^^ { case f ~ e => makeBin(f, e) }

  protected lazy val primary: PackratParser[Expr] = valMinus | valWithNot | ret | branchExpr | classExpr | moduleExpr | defExpr |
    ary | hash | aref | string | methodCall | literal  | bool | variable | "(" ~> expr <~ ")"

  protected lazy val arg: PackratParser[Expr] = assign | binary | methodCallNot | primary

  protected lazy val expr: PackratParser[Expr] = ret | CommadCallNot | commadCall | arg

  protected lazy val stmnt_value: PackratParser[Expr] = postModifier | assign | expr
  protected var stmnt: PackratParser[Expr] = stmnt_value

  override protected def stmnts: Parser[Stmnts] = (stmnt <~ (EOL | ";")).* ^^ Stmnts

  protected def makeBin(lh: Expr, rh: List[(Op, Expr)]): Expr = {
    innerMakeBin(lh, rh, 0) match {
      case (Nil, expr) => expr
      case (e, expr) => throw new Exception(e.toString)
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
