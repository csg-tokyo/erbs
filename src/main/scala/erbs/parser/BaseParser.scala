package erbs.parser

import scala.util.parsing.combinator.{RegexParsers, PackratParsers}

trait BaseParser[T] extends RegexParsers with PackratParsers {
  protected def commentLiteral: String
  protected def stmnts: Parser[T]

  def parse(in: String): Either[String, T] = {
    parseAll(stmnts, preprocess(in)) match {
      case Success(d, next) => Right(d)
      case NoSuccess(errorMsg, next) =>
        Left(s"$errorMsg: in ${next.pos.line} at column ${next.pos.column}")
    }
  }

  private def preprocess(body: String) =
    body.lines
      .map(trimComment.andThen(trimSpace).andThen(addNewLine))
      .filter(_ != "\n").mkString

  private val addNewLine: String => String = _ + "\n"

  private val trimSpace: String => String = _.trim

  private val trimComment =  { l: String =>
    val i = l.indexOf(commentLiteral)
    if (i != -1) l.take(i) else l
  }

  /*
   * RegexParser consumes space characters automaticaly (https://github.com/scala/scala-parser-combinators/blob/1.0.x/shared/src/main/scala/scala/util/parsing/combinator/RegexParsers.scala#L74).
   * customLiteral is used in order to avoid consuming space charactors.
   * `customLiteral(' ') #=> eed a space charactor`
   */
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
