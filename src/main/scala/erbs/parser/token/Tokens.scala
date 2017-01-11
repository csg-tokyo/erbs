package erbs.parser.token

trait Tokens {
  val T_INT = """(0|[1-9][0-9]*)""".r
  val T_DOUBLE = """(0|[1-9][0-9]*)\.[0-9]+""".r
  val T_MNAME = """[_a-zA-Z]*[a-zA-Z][_a-zA-Z0-9]*[!?]?""".r
  val T_DEFMNAME = """[_a-zA-Z]*[a-zA-Z][_a-zA-Z0-9]*[!?=]?""".r
  val T_STRING = """"(\\"|\\n|[^"])*"""".r
  val T_STRING_SINGLE = """'(\\'|\\n|[^'])*'""".r
  val T_ID       = """_*[a-z][_a-zA-Z0-9]*""".r
  val T_SYMBOL   = """_*[a-zA-Z][_a-zA-Z0-9]*""".r
  val T_CONSTANT = """[A-Z][_A-Za-z0-9]*""".r

  val K_DO = """do\b""".r
  val K_WHILE = """while\b""".r
  val K_UNTIL = """until\b""".r
  val K_CLS = """class\b""".r
  val K_MODULE = """module\b""".r
  val K_DEF = """def\b""".r
  val K_END = """end\b""".r
  val K_IF = """if\b""".r
  val K_UNLESS = """unless\b""".r
  val K_THEN = """then\b""".r
  val K_ELSE = """else\b""".r
  val K_ELSIF = """elsif\b""".r
  val K_TRUE = """true\b""".r
  val K_FALSE = """false\b""".r
  val K_RETURN =  """return\b""".r
  val K_SELF = """self\b""".r

  val T_HASH = "#"
}
