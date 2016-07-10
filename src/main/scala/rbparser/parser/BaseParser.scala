package rbparser.parser

import scala.util.parsing.combinator.{RegexParsers, PackratParsers}

trait BaseParser[T] extends RegexParsers with PackratParsers {
  protected def commentLiteral: String
  protected def stmnts: PackratParser[T]

  def parse(in: String): Either[String, T] = {
    parseAll(stmnts, preprocess(in)) match {
      case Success(d, next) => Right(d)
      case NoSuccess(errorMsg, next) =>
        Left(s"$errorMsg: in ${next.pos.line} at column ${next.pos.column}")
    }
  }

  protected def preprocess(body: String) = {
    val lines = body.split("\n").toSeq.map {
      line => trimSpace(removeComment(line))
    }
    removeEmptyLine(lines).mkString("\n") + "\n"
  }

  protected def removeEmptyLine(lines: Seq[String]): Seq[String] = lines.filter(_.size > 0)

  protected def removeComment(line: String) = {
    val i = line.indexOf(commentLiteral)
    if (i >= 0) line.take(i) else line
  }

  protected def trimSpace(line: String) = line.trim

  // Avoid consuming whitespace from RegexParser
  // if you write `customLiteral(" ")`, this space is needed as literal
  protected def customLiteral(s: String): Parser[String] = new Parser[String] {
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

}
