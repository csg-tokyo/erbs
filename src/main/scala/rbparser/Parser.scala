package rbparser

import scala.util.parsing.combinator.{RegexParsers, PackratParsers}

class Parser extends RegexParsers with PackratParsers with Tokens {
  def parse(in: String): Either[String, Prog] = {
    parseAll(prog, in) match {
      case Success(d, next) => Right(d)
      case NoSuccess(errorMsg, next) =>
        Left(s"$errorMsg : in ${next.pos.line} at column ${next.pos.column}")
    }
  }

  private lazy val EOL = "\n" | "\r\n" | "\r" | SCOLON
  private lazy val int: PackratParser[IntLit] = INT ^^ { case e => IntLit(e.toInt) }
  private lazy val double: PackratParser[DoubleLit] = DOUBLE ^^ { case e => DoubleLit(e.toDouble) }
  private lazy val id: PackratParser[IdLit] = ID ^^ { case e => IdLit(e) }
  private lazy val bool: PackratParser[BoolLit] = TRUE ^^ { case _ => BoolLit(true) } | FALSE ^^ { case _ => BoolLit(false) }

  private lazy val expr: PackratParser[Literal] = int | double | id | bool
  private lazy val prog: PackratParser[Prog] = (expr <~ EOL).* ^^ { case e => Prog(e) }
}
