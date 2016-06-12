package rbparser
package parser

import scala.util.matching.Regex
import scala.util.parsing.combinator.{RegexParsers, PackratParsers}

trait RubyParser extends RegexParsers with PackratParsers with rbparser.Tokens {
  def parse(in: String): Either[String, Stmnts] = {
    parseAll(stmnts, preprocess(in)) match {
      case Success(d, next) => Right(d)
      case NoSuccess(errorMsg, next) =>
        Left(s"$errorMsg: in ${next.pos.line} at column ${next.pos.column}")
    }
  }

  private def preprocess(txt: String) = {
    val lines = txt.split("\n").toSeq.map {
      line => trimSpace(removeComment(line))
    }
    removeEmptyLine(lines).mkString("\n") + "\n"
  }

  private def removeEmptyLine(lines: Seq[String]): Seq[String] = lines.filter(_.size > 0)

  private def removeComment(line: String) = {
    val i = line.indexOf(T_HASH)
    if (i >= 0) line.take(i) else line
  }

  private def trimSpace(line: String) = line.trim

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
  protected lazy val t_space: PackratParser[String] = customLiteral(" ")

  // hack to overrride primary value at subclass @ http://www.scala-lang.org/old/node/11315.html
  protected def reserved = reserved_value
  protected lazy val reserved_value = K_CLS | K_DEF | K_END | K_IF | K_THEN | K_ELSE | K_TRUE | K_FALSE | K_DO | K_RETURN | K_MODULE | K_UNLESS

  protected lazy val operator: PackratParser[Op] = t_plus | t_minus | t_ast | t_div | t_and | t_or | t_ge | t_gt | t_le | t_lt
  protected lazy val int: PackratParser[IntLit] = T_INT ^^ { case e => IntLit(e.toInt) }
  protected lazy val double: PackratParser[DoubleLit] = T_DOUBLE ^^ { case e => DoubleLit(e.toDouble) }
  protected lazy val string: PackratParser[StringLit] = (T_STRING | T_STRING_SINGLE) ^^ StringLit
  // `not` method reuturns `()` if succes, this term does not consume tokens
  protected lazy val lvar: PackratParser[LVar] = not(reserved) ~> T_ID ^^ LVar
  protected lazy val ivar: PackratParser[IVar] = "@" ~> T_SYMBOL ^^ IVar
  protected lazy val const: PackratParser[ConstLit] = T_CONSTANT ^^ ConstLit
  protected lazy val bool: PackratParser[BoolLit] = "true" ^^ { case _ => BoolLit(true) } | "false" ^^ { case _ => BoolLit(false) }
  protected lazy val symbol: PackratParser[SymbolLit] = ":" ~> T_SYMBOL ^^ SymbolLit

  protected lazy val valWithNot: PackratParser[Unary] = ("!" ~ (bool | const | lvar | ivar | "(" ~> expr <~ ")" | valWithNot) ^^ { case op ~ v => Unary(EXT(), v) })
  protected lazy val valMinus: PackratParser[Unary] = (t_minus ~ (const | lvar | ivar | double | int | ("(" ~> expr <~ ")"))) ^^ { case op ~ v => Unary(op, v) }
  protected lazy val literal: PackratParser[Expr] = symbol | double | int
  protected lazy val ret: PackratParser[Expr] = "return" ~> aArgs.? ^^ { case a => Return(a.getOrElse(Nil)) }

  // not double ref
  protected lazy val aref: PackratParser[ARef] = (primaryForAref <~ customLiteral("[")) ~ (primary <~ "]") ^^ { case v ~ ref => ARef(v, ref) }
  protected lazy val primaryForAref: PackratParser[Expr] = valMinus | valWithNot | ifExpr | string | userVar | "(" ~> expr <~ ")"
  protected lazy val ary: PackratParser[Ary] = "[" ~>  arefArgs.? <~ "]" ^^ { case args => Ary(args.getOrElse(Nil)) }

  protected lazy val hash: PackratParser[Hash] = "{" ~>  hashBody.? <~ "}" ^^ { case args =>  Hash(args.getOrElse(Map.empty)) }
  protected lazy val hashBody: PackratParser[Map[Expr, Expr]] = keyValues <~ ",".?
  protected lazy val keyValues: PackratParser[Map[Expr, Expr]] = (symbolKey | (arg <~ "=>")) ~ arg ~ ("," ~> keyValues).* ^^ {
    case k ~ v ~ Nil => Map(k -> v)
    case k ~ v ~ List(m) => Map(k -> v) ++ m
  }
  protected lazy val symbolKey: PackratParser[Expr] = ((string | T_SYMBOL ^^ SymbolLit) <~ ":")

  protected lazy val arefArgs: PackratParser[List[Expr]] = aArgs <~ ",".?

  protected lazy val userVar: PackratParser[Literal] = lvar | ivar | const

  //add inheritace
  protected lazy val classExpr: PackratParser[ClassExpr] = ("class" ~> const) ~ (stmnts <~ "end") ^^ { case name ~ body => ClassExpr(name, body) }
  protected lazy val moduleExpr: PackratParser[ClassExpr] = ("module" ~> const) ~ (stmnts <~ "end") ^^ { case name ~ body => ClassExpr(name, body) }

  protected lazy val defExpr: PackratParser[DefExpr] = ("def" ~> T_DEFMNAME) ~ formalArgs.? ~ (stmnts <~ "end") ^^ {
    case name ~ (None | Some(FormalArgs(Nil))) ~ body => DefExpr(name, None, body)
    case name ~ args ~ body => DefExpr(name, args, body)
  }

  protected lazy val fArgs: PackratParser[List[LVar]] = lvar ~ ("," ~> fArgs).* ^^ {
    case v ~ Nil => List(v)
    case v ~ List(ids) => v :: ids
  }

  protected lazy val aArgs: PackratParser[List[Expr]] = arg ~ ("," ~> aArgs).* ^^ {
    case v ~ Nil => List(v)
    case v ~ List(ids) => v :: ids
  }

  protected lazy val formalArgs: PackratParser[FormalArgs] =  "(" ~> fArgs.? <~ ")" ^^ { args => FormalArgs(args.getOrElse(Nil)) }
  protected lazy val actualArgs: PackratParser[ActualArgs] =  "(" ~> aArgs.? <~ ")" ^^ { args => ActualArgs(args.getOrElse(Nil)) }

  protected lazy val ifExpr: PackratParser[IfExpr] = ("if" ~> expr) ~ (stmnts <~ "end") ^^ { case cond ~ body => IfExpr(cond, body) }

  protected lazy val actualArgs2: PackratParser[ActualArgs] =  customLiteral("(") ~> aArgs.? <~ ")" ^^ { args => ActualArgs(args.getOrElse(Nil)) }

  protected lazy val methodCall: PackratParser[Call] = lvar ~ actualArgs2  ~ block.? ^^ {
    case LVar(name) ~ ActualArgs(Nil) ~ block => Call(None, name, None, block)
    case LVar(name) ~ args ~ block => Call(None, name, Some(args), block)
  } |
    (primary <~ ".") ~ T_MNAME ~ actualArgs2 ~ block.? ^^ {
      case recv ~ name ~ ActualArgs(Nil) ~ block => Call(Some(recv), name, None, block)
      case recv ~ name ~ args ~ block => Call(Some(recv), name, Some(args), block)
    }

  protected lazy val commandArgs: PackratParser[ActualArgs] = aArgs ^^ ActualArgs

  protected lazy val doBlock: PackratParser[Block] = ("do" ~> blockParamDef) ~ (stmnts <~ "end") ^^ {
    case params ~ body =>  DoBlock(params, body)
  }

  protected lazy val braceBlock: PackratParser[Block] = ("{" ~> blockParamDef) ~ (stmnt <~ "}") ^^ { //  FIX multi stmnt in one line
    case params ~ body =>  BraceBlock(params, Stmnts(List(body)))
  } | ("{" ~> blockParamDef) ~ (stmnts <~ "}") ^^ {
    case params ~ body =>  BraceBlock(params, body)
  }

  protected lazy val block: PackratParser[Block] = braceBlock | doBlock

  protected lazy val blockParamDef: PackratParser[Option[ActualArgs]] = ("|" ~> fArgs <~ "|").? ^^ {
    _.map(x => ActualArgs(x))
  }

  // TODO add COLON call e.g. a::b
  // command must havea at least one
  protected lazy val command: PackratParser[Cmd] = (lvar <~ t_space) ~ aArgs ~ block.? ^^ { // call args
    case LVar(name) ~ args ~ block => Cmd(None, name, Some(ActualArgs(args)), block)
  } | (primary <~ ".") ~ T_MNAME ~ (t_space ~> commandArgs).? ~ block.? ^^ { // a.call args
    case recv ~ name ~ Some(args) ~ block => Cmd(Some(recv), name, Some(args), block)
    case recv ~ name ~ None ~ block => Cmd(Some(recv), name, None, block)
  }

  protected lazy val commadCall: PackratParser[Expr] = T_MNAME ~ block ^^ {
    case name ~ block => Cmd(None, name, None, Some(block))
  } | command

  protected lazy val CommadCallNot: PackratParser[Expr] = "!" ~> commadCall ^^ { c => Unary(EXT(), c)}

  protected lazy val exprR: PackratParser[(Op, Expr)] = operator ~ primary ^^ { case op ~ f => (op, f) }

  protected lazy val lhs: PackratParser[Expr] = aref | (primary <~ ".") ~ (T_MNAME | const) ^^ {
    case rev ~ ConstLit(c) => Cmd(Some(rev), c, None, None)
    case rev ~ name => Cmd(Some(rev), name.toString(), None, None)
  }  | userVar

  // ignore double assign
  protected lazy val assign: PackratParser[Assign] = lhs ~ ("||=" | "&&=" | "=" | "+=" | "-=") ~ expr ^^ {
    case name ~ "=" ~ value => Assign(name, value, EQ())
    case name ~ "||=" ~ value => Assign(name, value, ORE())
    case name ~ "&&=" ~ value => Assign(name, value, ANDE())
    case name ~ "+=" ~ value => Assign(name, value, ADDE())
    case name ~ "-=" ~ value => Assign(name, value, SUBE())
  }

  protected lazy val postModifier: PackratParser[Expr] = expr ~ ("if" | "unless") ~ expr ^^ {
    case body ~ "if" ~ cond => IfModExpr(cond, body)
    case body ~ "unless" ~ cond => UnlessModExpr(cond, body)
  }

  protected lazy val binary: PackratParser[Expr] = arg ~ exprR.* ^^ { case f ~ e => makeBin(f, e) }

  protected lazy val primary: PackratParser[Expr] = valMinus | valWithNot | ret | ifExpr | classExpr | moduleExpr | defExpr |
  ary | hash | aref | string | methodCall | literal  | bool | userVar | "(" ~> expr <~ ")"

  protected lazy val arg: PackratParser[Expr] = assign | binary | "!" ~> methodCall ^^ { case c => Unary(EXT(), c)} | primary

  protected lazy val expr: PackratParser[Expr] = ret | CommadCallNot | commadCall | arg

  protected lazy val stmnt_value: PackratParser[Expr] = postModifier | assign | expr
  protected var stmnt: PackratParser[Expr] = stmnt_value

  protected def stmnts: PackratParser[Stmnts] = (stmnt <~ (EOL | ";")).* ^^ Stmnts

  protected def makeBin(lh: Expr, rh: List[(Op, Expr)]): Expr = {
    innerMakeBin(lh, rh, 0) match {
      case (Nil, expr) => expr
      case (e, expr) => throw new Exception(e.toString())
    }
  }

  // be able to represent whitespace
  def customLiteral(s: String): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      var i = 0
      var j = offset

      while (i < s.length && j < source.length && s.charAt(i) == source.charAt(j)) {
        i += 1
        j += 1
      }
      if (i == s.length) {
        Success(source.subSequence(offset, j).toString, in.drop(i))
      } else {
        val found = if (offset == source.length()) "end of source" else "`"+source.charAt(offset)+"'"
        Failure("`"+s+"' expected but "+found+" found", in.drop(offset))
      }
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
