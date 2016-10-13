package erbs.parser.token

trait OperatorToken {
  val K_OPERATOR =  """Operator\b""".r
  val K_DEFS =  """defs\b""".r
  val K_AT_TOKEN =  """@token\b""".r
}
