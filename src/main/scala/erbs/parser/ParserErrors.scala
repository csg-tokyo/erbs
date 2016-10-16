package erbs.parser

trait ParserErrors {
  class InvalidCondition(message :String = null, cause :Throwable = null) extends RuntimeException(message, cause)
  class NoSuchParser(message :String = null, cause :Throwable = null) extends RuntimeException(message, cause)
}
