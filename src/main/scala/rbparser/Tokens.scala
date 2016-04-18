package rbparser

trait Tokens {
  val T_INT = """(0|[1-9][0-9]*)""".r
  val T_DOUBLE = """(0|[1-9][0-9]*)\.[0-9]+""".r
  val T_ID = """_?[a-z][_a-zA-Z0-9]*""".r
  val T_CONSTANT = """[A-Z][_A-Z0-9]*""".r
  val T_INSTANCE_VAR = """@_?[a-zA-Z][_a-zA-Z0-9]*""".r
  val T_MNAME = """[_a-zA-Z]*[a-zA-Z][_a-zA-Z0-9]*\??""".r
  val T_STRING = """"(\"|\n|[^\"])*"""".r

  val T_COMMA = ","
  val T_DOT = "."
  val T_COLON = ":"
  val T_SCOLON = ";"
  val T_LPAREN = "("
  val T_RPAREN = ")"
  val T_LB = "["
  val T_RB = "]"
  val T_LBR = "{"
  val T_RBR = "}"
  val T_EQ = "="
  val T_LP = "{"
  val T_RP = "}"
  val T_PLUS = "+"
  val T_MINS = "-"
  val T_AST = "*"
  val T_DIV = "/"
  val T_GT = ">"
  val T_GE = ">="
  val T_LT = "<"
  val T_LE = "<="
  val T_OR = "||"
  val T_ORE = "||="
  val T_AND = "&&"
  val T_EX = "!"

  val T_DO = "do"
  val T_CLS = "class"
  val T_DEF = "def"
  val T_END = "end"
  val T_IF = "if"
  val T_THEN = "then"
  val T_ELSE = "else"

  val T_TRUE = "true"
  val T_FALSE = "false"

  val K_DO = """do\b""".r
  val K_CLS = """class\b""".r
  val K_DEF = """def\b""".r
  val K_END = """end\b""".r
  val K_IF = """if\b""".r
  val K_THEN = """then\b""".r
  val K_ELSE = """else\b""".r

  val K_TRUE = """true\b""".r
  val K_FALSE = """false\b""".r
}
