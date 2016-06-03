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

  protected lazy val operator: PackratParser[Op] = t_plus | t_minus | t_ast | t_div | t_and | t_or | t_ge | t_gt | t_le | t_lt
  protected lazy val reserved = K_CLS | K_DEF | K_END | K_IF | K_THEN | K_ELSE | K_TRUE | K_FALSE | K_DO | K_RETURN | K_MODULE | K_UNLESS
  protected lazy val int: PackratParser[IntLit] = T_INT ^^ { case e => IntLit(e.toInt) }
  protected lazy val double: PackratParser[DoubleLit] = T_DOUBLE ^^ { case e => DoubleLit(e.toDouble) }
  protected lazy val string: PackratParser[StringLit] = (T_STRING | T_STRING_SINGLE) ^^ StringLit
  // `not` method reuturns `()` if succes, this term does not consume tokens
  protected lazy val lvar: PackratParser[LVar] = not(reserved) ~> T_ID ^^ LVar
  protected lazy val ivar: PackratParser[IVar] = T_AT ~> T_SYMBOL ^^ IVar
  protected lazy val const: PackratParser[ConstLit] = T_CONSTANT ^^ ConstLit
  protected lazy val bool: PackratParser[BoolLit] = T_TRUE ^^ { case _ => BoolLit(true) } | T_FALSE ^^ { case _ => BoolLit(false) }
  protected lazy val symbol: PackratParser[SymbolLit] = T_COLON ~> T_SYMBOL ^^ SymbolLit

  protected lazy val valWithNot: PackratParser[Unary] = (T_EX ~ (bool | const | lvar | ivar | T_LPAREN ~> expr <~ T_RPAREN | valWithNot) ^^ { case op ~ v => Unary(EXT(), v) })
  protected lazy val valMinus: PackratParser[Unary] = (t_minus ~ (const | lvar | ivar | double | int | (T_LPAREN ~> expr <~ T_RPAREN))) ^^ { case op ~ v => Unary(op, v) }
  protected lazy val literal: PackratParser[Expr] = symbol | double | int
  protected lazy val ret: PackratParser[Expr] = T_RETURN ~> aArgs.? ^^ { case a => Return(a.getOrElse(Nil)) }

  // not double ref
  protected lazy val aref: PackratParser[ARef] = (primaryForAref <~ customLiteral("[")) ~ (primary <~ "]") ^^ { case v ~ ref => ARef(v, ref) }
  protected lazy val primaryForAref: PackratParser[Expr] = valMinus | valWithNot | ifExpr | string | userVar | T_LPAREN ~> expr <~ T_RPAREN
  protected lazy val ary: PackratParser[Ary] = "[" ~>  arefArgs.? <~ "]" ^^ { case args => Ary(args.getOrElse(Nil)) }

  protected lazy val hash: PackratParser[Hash] = "{" ~>  hashBody.? <~ "}" ^^ { case args =>  Hash(args.getOrElse(Map.empty)) }
  protected lazy val hashBody: PackratParser[Map[Expr, Expr]] = keyValues <~ T_COMMA.?
  protected lazy val keyValues: PackratParser[Map[Expr, Expr]] = (symbolKey | (arg <~ T_ROCKET)) ~ arg ~ (T_COMMA ~> keyValues).* ^^ {
    case k ~ v ~ Nil => Map(k -> v)
    case k ~ v ~ List(m) => Map(k -> v) ++ m
  }
  protected lazy val symbolKey: PackratParser[Expr] = ((string | T_SYMBOL ^^ SymbolLit) <~ T_COLON)

  protected lazy val arefArgs: PackratParser[List[Expr]] = aArgs <~ T_COMMA.?

  protected lazy val userVar: PackratParser[Literal] = lvar | ivar | const

  //add inheritace
  protected lazy val classExpr: PackratParser[ClassExpr] = (T_CLS ~> const) ~ (stmnts <~ T_END) ^^ { case name ~ body => ClassExpr(name, body) }
  protected lazy val moduleExpr: PackratParser[ClassExpr] = (T_MODULE ~> const) ~ (stmnts <~ T_END) ^^ { case name ~ body => ClassExpr(name, body) }

  protected lazy val defExpr: PackratParser[DefExpr] = (T_DEF ~> T_MNAME) ~ fomalArgs.? ~ (stmnts <~ T_END) ^^ {
    case name ~ (None | Some(FormalArgs(Nil))) ~ body => DefExpr(name, None, body)
    case name ~ args ~ body => DefExpr(name, args, body)
  }

  protected lazy val fArgs: PackratParser[List[LVar]] = lvar ~ (T_COMMA ~> fArgs).* ^^ {
    case v ~ Nil => List(v)
    case v ~ List(ids) => v :: ids
  }

  protected lazy val aArgs: PackratParser[List[Expr]] = arg ~ (T_COMMA ~> aArgs).* ^^ {
    case v ~ Nil => List(v)
    case v ~ List(ids) => v :: ids
  }

  protected lazy val fomalArgs: PackratParser[FormalArgs] =  T_LPAREN ~> fArgs.? <~ T_RPAREN ^^ { args => FormalArgs(args.getOrElse(Nil)) }
  protected lazy val actualArgs: PackratParser[ActualArgs] =  T_LPAREN ~> aArgs.? <~ T_RPAREN ^^ { args => ActualArgs(args.getOrElse(Nil)) }

  protected lazy val ifExpr: PackratParser[IfExpr] = (T_IF ~> expr) ~ (stmnts <~ T_END) ^^ { case cond ~ body => IfExpr(cond, body) }

  protected lazy val actualArgs2: PackratParser[ActualArgs] =  customLiteral(T_LPAREN) ~> aArgs.? <~ T_RPAREN ^^ { args => ActualArgs(args.getOrElse(Nil)) }

  protected lazy val methodCall: PackratParser[Call] = lvar ~ actualArgs2  ~ block.? ^^ {
    case LVar(name) ~ ActualArgs(Nil) ~ block => Call(None, name, None, block)
    case LVar(name) ~ args ~ block => Call(None, name, Some(args), block)
  } |
    (primary <~ T_DOT) ~ T_MNAME ~ actualArgs2 ~ block.? ^^ {
      case recv ~ name ~ ActualArgs(Nil) ~ block => Call(Some(recv), name, None, block)
      case recv ~ name ~ args ~ block => Call(Some(recv), name, Some(args), block)
    }

  protected lazy val commandArgs: PackratParser[ActualArgs] = aArgs ^^ ActualArgs

  protected lazy val doBlock: PackratParser[Block] = (K_DO ~> blockParamDef) ~ (stmnts <~ K_END) ^^ {
    case params ~ body =>  DoBlock(params, body)
  }

  protected lazy val braceBlock: PackratParser[Block] = (T_LBR ~> blockParamDef) ~ (stmnt <~ T_RBR) ^^ { //  FIX multi stmnt in one line
    case params ~ body =>  BraceBlock(params, Stmnts(List(body)))
  } |
    (T_LBR ~> blockParamDef) ~ (stmnts <~ T_RBR) ^^ {
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
    } | // a.call args
      (primary <~ T_DOT) ~ T_MNAME ~ (t_space ~> commandArgs).? ~ block.? ^^ {
        case recv ~ name ~ Some(args) ~ block => Cmd(Some(recv), name, Some(args), block)
        case recv ~ name ~ None ~ block => Cmd(Some(recv), name, None, block)
      }

  protected lazy val commadCall: PackratParser[Expr] = T_MNAME ~ block ^^ {
    case name ~ block => Cmd(None, name, None, Some(block))
  } | command

  protected lazy val CommadCallNot: PackratParser[Expr] = T_EX ~> commadCall ^^ { c => Unary(EXT(), c)}

  protected lazy val exprR: PackratParser[(Op, Expr)] = operator ~ primary ^^ { case op ~ f => (op, f) }

  protected lazy val lhs: PackratParser[Expr] = aref | (primary <~ T_DOT) ~ (T_MNAME | const) ^^ {
    case rev ~ ConstLit(c) => Cmd(Some(rev), c, None, None)
    case rev ~ name => Cmd(Some(rev), name.toString(), None, None)
  }  | userVar

  // ignore double assign
  protected lazy val assign: PackratParser[Assign] = lhs ~ (T_OREQ | T_ANDEQ | T_EQ | T_ADDEQ | T_SUBEQ) ~ expr ^^ {
    case name ~ T_EQ ~ value => Assign(name, value, EQ())
    case name ~ T_OREQ ~ value => Assign(name, value, ORE())
    case name ~ T_ANDEQ ~ value => Assign(name, value, ANDE())
    case name ~ T_ADDEQ ~ value => Assign(name, value, ADDE())
    case name ~ T_SUBEQ ~ value => Assign(name, value, SUBE())
  }

  protected lazy val postModifier: PackratParser[Expr] = expr ~ (T_IF | T_UNLESS) ~ expr ^^ {
    case body ~ T_IF ~ cond => IfModExpr(cond, body)
    case body ~ T_UNLESS ~ cond => UnlessModExpr(cond, body)
  }

  protected lazy val binary: PackratParser[Expr] = arg ~ exprR.* ^^ { case f ~ e => makeBin(f, e) }

  protected lazy val primary: Parser[Expr] = valMinus | valWithNot | ret | ifExpr | classExpr | moduleExpr | defExpr |
  ary | hash | aref | string | methodCall | literal  | bool | userVar | T_LPAREN ~> expr <~ T_RPAREN

  protected lazy val arg: PackratParser[Expr] = assign | binary | T_EX ~> methodCall ^^ { case c => Unary(EXT(), c)} | primary

  protected lazy val expr: PackratParser[Expr] = ret | CommadCallNot | commadCall | arg

  protected lazy val stmnt: PackratParser[Expr] = postModifier | assign | expr

  protected lazy val stmnts: PackratParser[Stmnts] = (stmnt <~ (EOL | T_SCOLON)).* ^^ Stmnts

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
