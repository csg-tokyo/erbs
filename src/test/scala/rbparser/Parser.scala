package rbparser

import org.scalatest._

class ParserTest extends FunSpec {

  /*  I will implement someday, So now it does not work following exmaples.
   *
   * parse("a = a = 1 + 2") { v => assert(v == Assign(LVar("a"), Assign(LVar("a"), Binary(PLUS(), IntLit(1), IntLit(2))))) }
   * parse("""@a[i][10] = "asdf"""") { v => assert(v == Assign(ARef(ARef(IVar("a"), LVar("i")), IntLit(10)), StringLit(""""asdf""""))) }
   * parse("@a.call + 10") { v => assert(v == Cmd(Some(IVar("a")), MethodName("call"), None, None)) }
   * parse("!a.call(10)") { v => assert(v == Unary(EXT(),Call(Some(LVar("a")), MethodName("call"), Some(ActualArgs(List(IntLit(10))))))) }
   *
   */

  describe ("Literal") {
    it ("parses Int Literal") {
      parse("1") { v => assert(v == IntLit(1)) }
      parse("100") { v => assert(v == IntLit(100)) }
    }

    it ("parses Double Literal") {
      parse("3.14") { v => assert(v == DoubleLit(3.14)) }
      parse("0.34") { v => assert(v == DoubleLit(0.34)) }
    }

    it ("parses Id Literal") {
      parse("x") { v => assert(v == LVar("x")) }
      parse("x2") { v => assert(v == LVar("x2")) }
      parse("ends") { v => assert(v == LVar("ends")) }
      parse("_end") { v => assert(v == LVar("_end")) }
      parse("end_") { v => assert(v == LVar("end_")) }
    }

    it ("parses String Literal") {
      parse(""""asdf"""") { v =>  assert(v == StringLit("\"asdf\"")) }
      parse(""""asdf\n"""") { v =>  assert(v == StringLit("\"asdf\\n\"")) }
      parse(""""as\ndf\n"""") { v =>  assert(v == StringLit("\"as\\ndf\\n\"")) }
      parse(""""as\"df"""") { v =>  assert(v == StringLit("\"as\\\"df\"")) }
      parse("""1 * "asdf" + "asdf2" * 2""") { v =>  assert(v == Binary(PLUS(),Binary(AST(),IntLit(1),StringLit("\"asdf\"")),Binary(AST(),StringLit("\"asdf2\""),IntLit(2))) ) }
    }

    it ("parses String Literal with single quoate") {
      parse("""'asdf'""") { v =>  assert(v == StringLit("'asdf'")) }
      parse("""'as\ndf\n'""") { v =>  assert(v == StringLit("'as\\ndf\\n'")) }
      parse("""'as"df'""") { v =>  assert(v == StringLit("'as\"df'")) }
      parse("""'as\'df'""") { v =>  assert(v == StringLit("'as\\'df'")) }
    }

    it ("parses BoolLit wrapped value") {
      parse("true") { v => assert(v == BoolLit(true)) }
    }

    it ("parses Const wrapped value") {
      parse("ASDF") { v =>  assert(v == ConstLit("ASDF")) }
      parse("ASDF2") { v => assert(v == ConstLit("ASDF2")) }
      parse("Aasd") { v => assert(v == ConstLit("Aasd")) }
    }

    it ("parses symbol wrapped value") {
      parse(":a") { v =>  assert(v == SymbolLit("a")) }
      parse(":a1") { v =>  assert(v == SymbolLit("a1")) }
      parse(":_a1") { v =>  assert(v == SymbolLit("_a1")) }
      parse(":__a1") { v =>  assert(v == SymbolLit("__a1")) }
      parse(":A") { v =>  assert(v == SymbolLit("A")) }
    }

    it ("parses instance variable wrapped value") {
      parse("@a") { v =>  assert(v == IVar("a")) }
      parse("@a1") { v =>  assert(v == IVar("a1")) }
      parse("@_a") { v => assert(v == IVar("_a")) }
      parse("@_a1") { v => assert(v == IVar("_a1")) }
      parse("@__a1") { v => assert(v == IVar("__a1")) }
      parse("@A") { v => assert(v == IVar("A")) }
    }

    it ("parses array value") {
      parse("[]") { v =>  assert(v == Ary(Nil)) }
      parse("[1]") { v =>  assert(v == Ary(List(IntLit(1)))) }
      parse("""["asf",3]""") { v => assert(v == Ary(List(StringLit(""""asf""""), IntLit(3)))) }
    }

    it ("parses hash") {
      parse("{}") { v =>  assert(v == Hash(Map.empty)) }
      parse("{ :key => 1 }") { v =>  assert(v == Hash(Map(SymbolLit("key") -> IntLit(1)))) }
      parse("""{ "key1" => 1, :key2 => 2 }""") {
        v => assert(v == Hash(Map(StringLit("\"key1\"") -> IntLit(1), SymbolLit("key2") -> IntLit(2))))
      }

      parse("{ key: 1 }") { v =>  assert(v == Hash(Map(SymbolLit("key") -> IntLit(1)))) }
      parse("""{ "key": 1 }""") { v =>  assert(v == Hash(Map(StringLit("\"key\"") -> IntLit(1)))) }
      parse("""{ key1: "value1", key2: "value2" }""") { v =>
        assert(v == Hash(Map(SymbolLit("key1") -> StringLit("\"value1\""), SymbolLit("key2") -> StringLit("\"value2\""))))
      }
    }
  }

  describe("stmnt") {
    describe("Assigns") {
      it ("parses simple assigns") {
        parse("a = 1 + 2") { v => assert(v == Assign(LVar("a"), Binary(PLUS(), IntLit(1), IntLit(2)), EQ())) }
        parse("@a = 1 + 2") { v => assert(v == Assign(IVar("a"), Binary(PLUS(), IntLit(1), IntLit(2)), EQ())) }
        parse("@a = :keyword") { v => assert(v == Assign(IVar("a"),SymbolLit("keyword"), EQ())) }
      }

      it ("parses assings stmnt") {
        parse("a = @a.call") { v => assert(v == Assign(LVar("a"), Cmd(Some(IVar("a")), "call", None, None), EQ())) }
        parse("""@a.size = 10""") { v => assert(v == Assign(Cmd(Some(IVar("a")) ,"size", None, None), IntLit(10), EQ())) }
        parse("""@a[i] = "asdf"""") { v => assert(v == Assign(ARef(IVar("a"), LVar("i")), StringLit(""""asdf""""), EQ())) }
        parse("@a ||= 1 + 2") { v => assert(v == Assign(IVar("a"), Binary(PLUS(), IntLit(1), IntLit(2)), ORE())) }
        parse("@a += 1 + 2") { v => assert(v == Assign(IVar("a"), Binary(PLUS(), IntLit(1), IntLit(2)), ADDE())) }
      }
    }

    it ("parses post modifier") {
      parse("10 if true") { v => assert(v == IfModExpr(BoolLit(true), IntLit(10))) }
      parse("return 10 if true") { v => assert(v == IfModExpr(BoolLit(true), Return(List(IntLit(10))))) }
      parse("a[i] unless a.hash?") { v => assert(v == UnlessModExpr(Cmd(Some(LVar("a")), "hash?", None, None), ARef(LVar("a"), LVar("i")))) }
    }
  }

  describe("expr") {
    it ("parses return stmnt") {
      parse("return") { v =>  assert(v == Return(Nil)) }
      parse("return a") { v =>  assert(v == Return(List(LVar("a")))) }
      parse("return a, 10") { v =>  assert(v == Return(List(LVar("a"), IntLit(10)))) }
    }

    it("parses command call") {
      parse("A.new") { v => assert(v == Cmd(Some(ConstLit("A")), "new", None, None)) }
      parse("@a.call") { v => assert(v == Cmd(Some(IVar("a")), "call", None, None)) }
      parse("@a.call 10, 20") { v => assert(v == Cmd(Some(IVar("a")), "call", Some(ActualArgs(List(IntLit(10), IntLit(20)))), None)) }
      parse("call 10") { v => assert(v == Cmd(None, "call", Some(ActualArgs(List(IntLit(10)))), None)) }
      parse("call 10 + 1") { v => assert(v == Cmd(None, "call", Some(ActualArgs(List(Binary(PLUS(), IntLit(10), IntLit(1))))), None)) }
      parse("attr_reader :a, :b") { v => assert(v == Cmd(None, "attr_reader", Some(ActualArgs(List(SymbolLit("a"), SymbolLit("b")))), None)) }
    }

    it("parses method call with { ~ } block") {
      parse("""call(10) { |x| x + 1 }""") { v =>
        assert(v == Call(
          None,
          "call",
          Some(ActualArgs(List(IntLit(10)))),
          Some(BraceBlock(Some(ActualArgs(List(LVar("x")))),
            Stmnts(List(Binary(PLUS(), LVar("x"),IntLit(1))))))))
      }

      parse("""a.call(10,11) { |x| x + 1 }""") { v =>
        assert(v == Call(
          Some(LVar("a")),
          "call",
          Some(ActualArgs(List(IntLit(10), IntLit(11)))),
          Some(BraceBlock(Some(ActualArgs(List(LVar("x")))),
            Stmnts(List(Binary(PLUS(),LVar("x"),IntLit(1))))))))
      }
    }

    it("parses method call with do ~ end block") {
      parse("""call(10, :symbol) do |x|
x + 1
end""") { v =>
        assert(v == Call(
          None,
          "call",
          Some(ActualArgs(List(IntLit(10), SymbolLit("symbol")))),
          Some(DoBlock(Some(ActualArgs(List(LVar("x")))),
            Stmnts(List(Binary(PLUS(),LVar("x"), IntLit(1))))))
        ))
      }

      parse("""a.call(10) do |x|
x + 1
end""") { v =>
        assert(v == Call(
          Some(LVar("a")),
          "call",
          Some(ActualArgs(List(IntLit(10)))),
          Some(DoBlock(Some(ActualArgs(List(LVar("x")))),
            Stmnts(List(Binary(PLUS(),LVar("x"), IntLit(1))))))
        ))
      }
    }

    it("parses command call with { ~ } block") {
      parse("""call { |x| x + 1 }""") { v =>
        assert(v == Cmd(
          None,
          "call",
          None,
          Some(BraceBlock(Some(ActualArgs(List(LVar("x")))),
            Stmnts(List(Binary(PLUS(),LVar("x"),IntLit(1))))))))
      }

      parse("""call :aaa { |x| x + 1 }""") { v =>
        assert(v == Cmd(
          None,
          "call",
          Some(ActualArgs(List(SymbolLit("aaa")))),
          Some(BraceBlock(Some(ActualArgs(List(LVar("x")))),
            Stmnts(List(Binary(PLUS(),LVar("x"),IntLit(1))))))))
      }
    }

    it("parses command call with do ~ end block") {
      parse("""call do |x|
x + 1
end""") { v =>
        assert(v == Cmd(
          None,
          "call",
          None,
          Some(DoBlock(Some(ActualArgs(List(LVar("x")))),
            Stmnts(List(Binary(PLUS(),LVar("x"),IntLit(1))))))))
      }

      parse("""call :visual do |x|
x + 1
end""") { v =>
        assert(v == Cmd(
          None,
          "call",
          Some(ActualArgs(List(SymbolLit("visual")))),
          Some(DoBlock(Some(ActualArgs(List(LVar("x")))),
            Stmnts(List(Binary(PLUS(), LVar("x"), IntLit(1))))))))
      }

      parse("""[1,2].each do |x|
a + 1
end""") { v =>
        assert(v == Cmd(
          Some(Ary(List(IntLit(1), IntLit(2)))),
          "each",
          None,
          Some(
            DoBlock(Some(ActualArgs(List(LVar("x")))),
              Stmnts(List(Binary(PLUS(), LVar("a"), IntLit(1))))))
        ))
      }

      parse("""A.new do
a + 1
end""") { v => assert(v == Cmd(Some(ConstLit("A")), "new", None, Some(
  DoBlock(None,Stmnts(List(Binary(PLUS(), LVar("a"),IntLit(1)))))))) }
    }
  }

  describe("primary") {
    it("parses exclamation prefix") {
      parse("!@a") { v =>  assert(v == Unary(EXT(), IVar("a"))) }
      parse("!true") { v =>  assert(v == Unary(EXT(), BoolLit(true))) }
      parse("!a") { v =>  assert(v == Unary(EXT(), LVar("a"))) }
      parse("!A") { v =>  assert(v == Unary(EXT(), ConstLit("A"))) }
      parse("!a.call") { v =>  assert(v == Unary(EXT(), Cmd(Some(LVar("a")), "call", None, None))) }
      parse("!(true && false)") { v =>  assert(v == Unary(EXT(), Binary(AND(), BoolLit(true), BoolLit(false)))) }
      parse("!true && false") { v =>  assert(v == Binary(AND(),Unary(EXT(),BoolLit(true)),BoolLit(false))) }
      parse("!!a") { v =>  assert(v == Unary(EXT(), Unary(EXT(), LVar("a")))) }
    }

    it("parese minus prefix") {
      parse("-a") { v =>  assert(v == Unary(MINUS(), LVar("a"))) }
      parse("-A") { v =>  assert(v == Unary(MINUS(), ConstLit("A"))) }
      parse("-10") { v =>  assert(v == Unary(MINUS(), IntLit(10))) }
      parse("-10.0") { v =>  assert(v == Unary(MINUS(), DoubleLit(10.0))) }
      parse("-(1+2)") { v =>  assert(v == Unary(MINUS(), Binary(PLUS(), IntLit(1), IntLit(2)))) }
    }

    it("parses array ref") {
      parse("a[10]") { v =>  assert(v == ARef(LVar("a"), IntLit(10))) }
      parse("@abc[10]") { v =>  assert(v == ARef(IVar("abc"), IntLit(10))) }
      parse("@abc[i]") { v =>  assert(v == ARef(IVar("abc"), LVar("i"))) }
      parse("[1,2].each") { v =>  assert(v == Cmd(Some(Ary(List(IntLit(1), IntLit(2)))), "each", None, None)) }
    }

    it("parses method call") {
      parse("call(a)") { v => assert(v == Call(None, "call", Some(ActualArgs(List(LVar("a")))), None)) }
      parse("call(a) + 1") { v => assert(v ==  Binary(PLUS(), Call(None, "call", Some(ActualArgs(List(LVar("a")))), None), IntLit(1))) }
      parse("b.call(a)") { v => assert(v == Call(Some(LVar("b")), "call", Some(ActualArgs(List(LVar("a")))), None)) }
      parse("a1._call(10)") { v => assert(v == Call(Some(LVar("a1")), "_call", Some(ActualArgs(List(IntLit(10)))), None)) }
      parse("a1._calL?(10)") { v => assert(v == Call(Some(LVar("a1")), "_calL?", Some(ActualArgs(List(IntLit(10)))), None)) }
      parse("a1._calL!(10)") { v => assert(v == Call(Some(LVar("a1")), "_calL!", Some(ActualArgs(List(IntLit(10)))), None)) }
      parse("a1.call()") { v => assert(v == Call(Some(LVar("a1")), "call", Some(ActualArgs(List())), None)) }
      parse("a.call() < 10") { v => assert(v == Binary(LT(), Call(Some(LVar("a")), "call", Some(ActualArgs(List())), None), IntLit(10))) }
      parse("a1.call(10, true)") { v => assert(v == Call(Some(LVar("a1")), "call", Some(ActualArgs(List(IntLit(10), BoolLit(true)))), None)) }
    }

    it ("parses if expression") {
      parse("""if  true
  1 + 2
end""") { v => assert(v == IfExpr(BoolLit(true), Stmnts(List(Binary(PLUS(),IntLit(1),IntLit(2)))))) }
      parse("""if a(10)
  b
end""") { v => assert(v == IfExpr(Call(None, "a", Some(ActualArgs(List(IntLit(10)))), None), Stmnts(List(LVar("b"))))) }
    parse("""unless a(10)
  b
end""") { v => assert(v == UnlessExpr(Call(None, "a", Some(ActualArgs(List(IntLit(10)))), None), Stmnts(List(LVar("b"))))) }
    }

    it ("parses class") {
      parse("""class Sample
end""") { v => assert(v == ClassExpr(ConstLit("Sample"), Stmnts(List()))) }

      parse("""class A
  def a
    1 + 2
  end
end
  """) { v => assert(v == ClassExpr(ConstLit("A"), Stmnts(List(
    DefExpr("a", None, Stmnts(List(Binary(PLUS(), IntLit(1), IntLit(2)))))))))
      }
    }

    it ("parses def") {
      parse("""def a
end""") { v => assert(v == DefExpr("a", None, Stmnts(List())))}

      parse("""def a?
end""") { v => assert(v == DefExpr("a?", None, Stmnts(List())))}

      parse("""def ASDF?
end""") { v => assert(v == DefExpr("ASDF?", None, Stmnts(List())))}

      parse("""def _a?
end""") { v => assert(v == DefExpr("_a?", None, Stmnts(List())))}

      parse("""def a?()
end""") { v => assert(v == DefExpr("a?", Some(FormalArgs(Nil)), Stmnts(List())))}

      parse("""def a()
end""") { v => assert(v == DefExpr("a", Some(FormalArgs(Nil)), Stmnts(List())))}

      parse("""def a(opt)
end""") { v => assert(v == DefExpr("a", Some(FormalArgs(List(LVar("opt")))), Stmnts(List())))}

      parse("""def call(a, b)
end""") { v => assert(v == DefExpr("call", Some(FormalArgs(List(LVar("a"), LVar("b")))), Stmnts(List())))}

      parse("""def call
  "1+2"
end""") { v => assert(v == DefExpr("call", None, Stmnts(List(StringLit(""""1+2""""))))) }

      parse("""def value=(v)
  @value = v
end""") { v => assert(v == DefExpr("value=", Some(FormalArgs(List(LVar("v")))), Stmnts(List(Assign(IVar("value"), LVar("v"), EQ()))))) }
    }
  }

  describe("arg") {
    it("parses Binary") {
      parse("1 + 2") { v => assert(v == Binary(PLUS(), IntLit(1), IntLit(2))) }
      parse("1 + -2") { v => assert(v == Binary(PLUS(), IntLit(1), Unary(MINUS(), IntLit(2)))) }
      parse("-2 + 1") { v => assert(v == Binary(PLUS(), Unary(MINUS(), IntLit(2)), IntLit(1))) }
      parse("1 + (2 + 2)") { v => assert(v == Binary(PLUS(), IntLit(1), Binary(PLUS(), IntLit(2), IntLit(2)))) }
      parse("1 - 2") { v => assert(v == Binary(MINUS(), IntLit(1), IntLit(2))) }
      parse("1 * 2") { v => assert(v == Binary(AST(), IntLit(1), IntLit(2))) }
      parse("1 / 2") { v => assert(v == Binary(DIV(), IntLit(1), IntLit(2))) }
      parse("i / 2") { v => assert(v == Binary(DIV(), LVar("i"), IntLit(2))) }
      parse("@a / 2") { v => assert(v == Binary(DIV(), IVar("a"), IntLit(2))) }
      parse("1 - 2 * 3") { v => assert(v == Binary(MINUS(), IntLit(1), Binary(AST(), IntLit(2), IntLit(3)))) }
      parse("1 - 2 - 3") { v => assert(v == Binary(MINUS(), Binary(MINUS(), IntLit(1), IntLit(2)), IntLit(3))) }

      parse("1 + 2 * 1 + 10") { v => assert(
        v == Binary(PLUS(), Binary(PLUS(), IntLit(1), Binary(AST(), IntLit(2), IntLit(1))), IntLit(10)))
      }
    }

    it ("parses compare binary") {
      parse("1 < 2") { v => assert(v == Binary(LT(), IntLit(1), IntLit(2))) }
      parse("1 >= 2") { v => assert(v == Binary(GE(), IntLit(1), IntLit(2))) }
      parse("1 + 2 >= 3") { v => assert(v == Binary(GE(), Binary(PLUS(), IntLit(1), IntLit(2)), IntLit(3))) }
    }

    it ("pares cond binary") {
      parse("true && false") { v => assert(v == Binary(AND(), BoolLit(true), BoolLit(false))) }
      parse("true || false && false") { v => assert(v == Binary(OR(), BoolLit(true), Binary(AND(), BoolLit(false), BoolLit(false)))) }
    }
  }

  describe ("comment") {
    it ("ignore the code of following '#' ") {
      parse("""def call
  "1+2" # shoud be ingore
end""") { v => assert(v == DefExpr("call", None, Stmnts(List(StringLit(""""1+2""""))))) }
    }
  }

  describe ("Extendable") {
    it ("pares operator_with") {
      parse("""operator_with(mod, origin)
  { x -> y } where { x: origin, y: origin } => { y = x }
end""") { v => assert(v == Operators(List(
  Operator(Set("mod", "origin"),
    Syntax(Map("x" -> LVar("origin"), "y" -> LVar("origin")), List("x", "->", "y")),
    Assign(LVar("y"), LVar("x"), EQ())))))
      }
    }

    describe ("when ellipsis where cond") {
      it ("pares operator_with") {
        assertParseResult(Operators(List(Operator(Set("origin"), Syntax(Map(), List("%")), IntLit(100))))) {
          """operator_with(origin)
  { % }  => { 100 }
end"""
        }

      }
    }

    describe ("when mutiple defition in operator_with") {
      it ("parses") {
        parse("""operator_with(mod, origin)
  { x -> y } where { x: origin, y: origin } => { y = x }
  { x <- y } where { x: origin, y: origin } => { x = y }
end
""") { v => assert(v == Operators(List(
  Operator(Set("mod", "origin"),
    Syntax(Map("x" -> LVar("origin"), "y" -> LVar("origin")), List("x", "->", "y")),
    Assign(LVar("y"), LVar("x"), EQ())),
  Operator(Set("mod", "origin"),
    Syntax(Map("x" -> LVar("origin"), "y" -> LVar("origin")), List("x", "<-", "y")),
    Assign(LVar("x"), LVar("y"), EQ())))))
        }
      }
    }

    describe ("when tag has condition") {
      it ("parses and, or") {
        assertResult(Operators(List(
          Operator(Set("origin"), Syntax(Map("x" -> Binary(OR(), LVar("origin"), LVar("mod"))), List("x", "<-", "1")), Assign(LVar("x"), IntLit(1), EQ())),
          Operator(Set("origin"), Syntax(Map("x" -> Binary(OR(), LVar("origin"), Binary(AND(), LVar("origin"), LVar("mod"))), "y" -> Binary(AND(), LVar("origin"), LVar("mod"))), List("x", "<-", "y")), Assign(LVar("x"), LVar("y"), EQ()))
        ))) {
          val parser = new Parser()
          parser.parse("""operator_with(mod, origin)
  { x <- 1 } where { x: origin } => { x = 1 }
end

operator_with(origin)
  { x <- 1 } where { x: origin || mod } => { x = 1 }
  { x <- y } where { x: origin || (origin && mod), y: origin && mod } => { x = y }
end""" + "\n") match {
            case Right(Stmnts(x)) => x(1)
            case Left(s) => throw new Exception(s)
          }
        }
      }

      it ("parses Not(!)") {
        assertResult(Cmd(None, "resources", Some(ActualArgs(List(StringLit("aws")))), None)) {
          parse2("""
operator_with(resource_name)
  { aws } => { "aws" }
end

operator_with(name)
  { foo  } => { "foo" }
end

operator_with(origin)
  { resources name  } where { name: !name && !origin } => { resources name }
end

resources aws
""")
        }
      }
    }
  }

  def assertParseResult(expect: Expr, cule: String = "")(body: String) = {
    val parser = new Parser()
    assertResult(Stmnts(List(expect))) {
      parser.parse(body + "\n") match {
        case Right(x) => x
        case Left(s) => throw new ParsingError(s)
      }
    }
  }

  protected class ParsingError(message :String = null, cause :Throwable = null) extends RuntimeException(message, cause)

  def parse2(in: String): Expr = (new Parser).parse(in + "\n") match {
    case Right(Stmnts(x)) => x.last
    case Left(s) => LVar(s)
  }

  def parse(x: String)(fn: Expr => Unit): Unit = {
    val parser = new Parser()
    parser.parse(x + "\n") match {
      case Right(Stmnts(x)) => fn(x(0))
      case Left(s) => throw new Exception(s)
        // case Right(x) => assert(x == 0)
        // case Left(s) => assert(s == "parsing fail")
    }
  }
}
