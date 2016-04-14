package rbparser

trait Tokens {
  val INT = """(0|[1-9][0-9]*)""".r
  val DOUBLE =  """(0|[1-9][0-9]*)\.[0-9]+""".r
  val ID = """[a-z][a-zA-Z0-9]*""".r
  val CONSTANT = """[A-Z][A-Z0-9]*""".r

  val COMMA = ","
  val COLON = ":"
  val SCOLON = ";"
  val LPAREN = "("
  val RPAREN = ")"
  val LB = "["
  val RB = "]"
  val LBR = "{"
  val RBR = "}"
  val EQ = "="
  val LP = "{"
  val RP = "}"
  val PLS = "+"
  val MINS = "-"
  val AST = "*"
  val DIV = "/"
  val BT = ">"
  val LT = "<"

  val FUN = "class"
  val DEF = "def"
  val END = "end"
  val IF = "if"
  val THEN = "then"
  val ELSE = "else"

  val TRUE = "true"
  val FALSE = "false"
}
