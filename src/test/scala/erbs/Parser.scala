package erbs

import erbs.parser.ast._

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
      assertResult(IntLit(1)) {
        parse("1")
      }
      assertResult(IntLit(100)) {
        parse("100")
      }
    }

    it ("parses Double Literal") {
      assertResult(DoubleLit(3.14)) {
        parse("3.14")
      }
      assertResult(DoubleLit(0.34)) {
        parse("0.34")
      }
    }

    it ("parses Id Literal") {
      assertResult(LVar("x")) {
        parse("x")
      }
      assertResult(LVar("x2")) {
        parse("x2")
      }
      assertResult(LVar("ends")) {
        parse("ends")
      }
      assertResult(LVar("_end")) {
        parse("_end")
      }
      assertResult(LVar("end_")) {
        parse("end_")
      }
    }

    it ("parses String Literal") {
      assertResult(StringLit("\"asdf\"")) {
        parse(""""asdf"""")
      }
      assertResult(StringLit("\"asdf\\n\"")) {
        parse(""""asdf\n"""")
      }
      assertResult(StringLit("\"as\\\"df\"")) {
        parse(""""as\"df"""")
      }
      assertResult(StringLit("\"as\\ndf\\n\"")) {
        parse(""""as\ndf\n"""")
      }
      assertResult(Binary(PLUS, Binary(MUL, IntLit(1), StringLit("\"asdf\"")), Binary(MUL, StringLit("\"asdf2\""), IntLit(2)))) {
        parse("""1 * "asdf" + "asdf2" * 2""")
      }
    }

    it ("parses String Literal with single quoate") {
      assertResult(StringLit("'asdf'")) {
        parse("'asdf'")
      }
      assertResult(StringLit("'as\\ndf\\n'")) {
        parse("""'as\ndf\n'""")
      }
      assertResult(StringLit("'as\"df'")) {
        parse("""'as"df'""")
      }
      assertResult(StringLit("'as\\'df'")) {
        parse("""'as\'df'""")
      }
    }

    it ("parses BoolLit wrapped value") {
      assertResult(BoolLit(true)) {
        parse("true")
      }
      assertResult(BoolLit(false)) {
        parse("false")
      }
    }

    it ("parses Const wrapped value") {
      assertResult(ConstLit("ASDF")) {
        parse("ASDF")
      }
      assertResult(ConstLit("ASDF2")) {
        parse("ASDF2")
      }
      assertResult(ConstLit("Sample")) {
        parse("Sample")
      }
    }

    it ("parses symbol wrapped value") {
      assertResult(SymbolLit("a"))    {
        parse(":a")
      }
      assertResult(SymbolLit("_a1"))  {
        parse(":_a1")
      }
      assertResult(SymbolLit("__a1")) {
        parse(":__a1")
      }
      assertResult(SymbolLit("A")) {
        parse(":A")
      }
      assertResult(SymbolLit("__A")) {
        parse(":__A")
      }
    }

    it ("parses instance variable wrapped value") {
      assertResult(IVar("a")) {
        parse("@a")
      }
      assertResult(IVar("a1")) {
        parse("@a1")
      }
      assertResult(IVar("_a")) {
        parse("@_a")
      }
      assertResult(IVar("_a1")) {
        parse("@_a1")
      }
      assertResult(IVar("__a1")) {
        parse("@__a1")
      }
      assertResult(IVar("A")) {
        parse("@A")
      }
    }

    it ("parses array value") {
      assertResult(Ary(Nil)) {
        parse("[]")
      }
      assertResult(Ary(List(IntLit(1)))) {
        parse("[1]")
      }
      assertResult(Ary(List(StringLit(""""a""""), IntLit(3)))) {
        parse("[\"a\", 3]")
      }
    }

    it ("parses hash") {
      assertResult(Hash(Map.empty)) {
        parse("{}")
      }
      assertResult(Hash(Map(SymbolLit("key") -> IntLit(1)))) {
        parse("{ :key => 1 }")
      }
      assertResult(Hash(Map(SymbolLit("key") -> IntLit(1)))) {
        parse("{ key: 1 }")
      }
      assertResult(Hash(Map(StringLit("\"key\"") -> IntLit(1)))) {
        parse("""{ "key": 1 }""")
      }
      assertResult(Hash(Map(StringLit("\"key1\"") -> IntLit(1), SymbolLit("key2") -> IntLit(2)))) {
        parse("""{ "key1" => 1, :key2 => 2 }""")
      }
      assertResult(Hash(Map(SymbolLit("key1") -> StringLit("\"value1\""), SymbolLit("key2") -> StringLit("\"value2\"")))) {
        parse("""{ key1: "value1", key2: "value2" }""")
      }
    }
  }

  describe("stmnt") {
    describe("Assigns") {
      it ("parses simple assigns") {
        assertResult(Assign(LVar("a"), Binary(PLUS, IntLit(1), IntLit(2)), EQ)) {
          parse("a = 1 + 2")
        }
        assertResult(Assign(IVar("a"), Binary(PLUS, IntLit(1), IntLit(2)), EQ)) {
          parse("@a = 1 + 2")
        }
        assertResult(Assign(IVar("a"),SymbolLit("keyword"), EQ)) {
          parse("@a = :keyword")
        }
      }

      it ("parses assings stmnt") {
        assertResult(Assign(LVar("a"), Cmd(Some(IVar("a")), "call", None, None), EQ)) {
          parse("a = @a.call")
        }
        assertResult(Assign(Cmd(Some(IVar("a")) ,"size", None, None), IntLit(10), EQ)) {
          parse("@a.size = 10")
        }
        assertResult(Assign(ARef(IVar("a"), LVar("i")), StringLit(""""asdf""""), EQ)) {
          parse("""@a[i] = "asdf"""")
        }
        assertResult(Assign(IVar("a"), Binary(PLUS, IntLit(1), IntLit(2)), ORE)) {
          parse("@a ||= 1 + 2")
        }
        assertResult(Assign(IVar("a"), Binary(PLUS, IntLit(1), IntLit(2)), ADDE)) {
          parse("@a += 1 + 2")
        }
      }
    }

    it ("parses post modifier") {
      assertResult(IfModExpr(BoolLit(true), IntLit(10))) {
        parse("10 if true")
      }
      assertResult(IfModExpr(BoolLit(true), Return(List(IntLit(10))))) {
        parse("return 10 if true")
      }
      assertResult(UnlessModExpr(Cmd(Some(LVar("a")), "hash?", None, None), ARef(LVar("a"), LVar("i")))) {
        parse("a[i] unless a.hash?")
      }
    }
  }

  describe ("Expr") {
    it ("parses return stmnt") {
      assertResult(Return(Nil)) {
        parse("return")
      }
      assertResult(Return(List(LVar("a")))) {
        parse("return a")
      }
      assertResult(Return(List(LVar("a"), IntLit(10)))) {
        parse("return a, 10")
      }
    }

    it ("parses command call") {
      assertResult(Cmd(Some(ConstLit("A")), "new", None, None)) {
        parse("A.new")
      }
      assertResult(Cmd(Some(IVar("a")), "call", None, None)) {
        parse("@a.call")
      }
      assertResult(Cmd(Some(IVar("a")), "call", Some(ActualArgs(List(IntLit(10), IntLit(20)))), None)) {
        parse("@a.call 10, 20")
      }
      assertResult(Cmd(None, "call", Some(ActualArgs(List(IntLit(10)))), None)) {
        parse("call 10")
      }
      assertResult(Cmd(None, "call", Some(ActualArgs(List(Binary(PLUS, IntLit(10), IntLit(1))))), None)) {
        parse("call 10 + 1")
      }
      assertResult(Cmd(None, "attr_reader", Some(ActualArgs(List(SymbolLit("a"), SymbolLit("b")))), None)) {
        parse("attr_reader :a, :b")
      }
    }

    it ("parses method call with { ~ } block") {
      assertResult(Call(None, "call",
        Some(ActualArgs(List(IntLit(10)))),
        Some(BraceBlock(Some(ActualArgs(List(LVar("x")))), Stmnts(List(Binary(PLUS, LVar("x"),IntLit(1)))))))) {
        parse("""call(10) { |x| x + 1 }""")
      }
      assertResult(Call(Some(LVar("a")), "call",
        Some(ActualArgs(List(IntLit(10), IntLit(11)))),
        Some(BraceBlock(Some(ActualArgs(List(LVar("x")))), Stmnts(List(Binary(PLUS,LVar("x"),IntLit(1)))))))) {
        parse("""a.call(10,11) { |x| x + 1 }""")
      }
    }

    it("parses method call with do ~ end block") {
      assertResult(Call(None, "call",
        Some(ActualArgs(List(IntLit(10), SymbolLit("symbol")))),
        Some(DoBlock(Some(ActualArgs(List(LVar("x")))), Stmnts(List(Binary(PLUS,LVar("x"), IntLit(1)))))))) {
        parse("""
call(10, :symbol) do |x|
  x + 1
end
""")
      }
      assertResult(Call(Some(LVar("a")), "call",
        Some(ActualArgs(List(IntLit(10)))),
        Some(DoBlock(Some(ActualArgs(List(LVar("x")))), Stmnts(List(Binary(PLUS,LVar("x"), IntLit(1))))))
      )) {
        parse("""
a.call(10) do |x|
  x + 1
end
""")
      }
    }

    it ("parses command call with { ~ } block") {
      assertResult(Cmd(None, "call", None,
        Some(BraceBlock(Some(ActualArgs(List(LVar("x")))), Stmnts(List(Binary(PLUS,LVar("x"),IntLit(1)))))))) {
        parse("""call { |x| x + 1 }""")
      }
      assertResult(Cmd(None, "call",
        Some(ActualArgs(List(SymbolLit("aaa")))),
        Some(BraceBlock(Some(ActualArgs(List(LVar("x")))), Stmnts(List(Binary(PLUS,LVar("x"),IntLit(1)))))))) {
        parse("""call :aaa { |x| x + 1 }""")
      }
    }

    it ("parses command call with do ~ end block") {
      assertResult(Cmd(None, "call", None,
        Some(DoBlock(Some(ActualArgs(List(LVar("x")))), Stmnts(List(Binary(PLUS,LVar("x"),IntLit(1)))))))) {
        parse("""
call do |x|
  x + 1
end
""")
      }
      assertResult(Cmd(None, "call",
        Some(ActualArgs(List(SymbolLit("visual")))),
        Some(DoBlock(Some(ActualArgs(List(LVar("x")))), Stmnts(List(Binary(PLUS, LVar("x"), IntLit(1)))))))) {
        parse("""
call :visual do |x|
    x + 1
    end
""")
      }
      assertResult(Cmd(Some(Ary(List(IntLit(1), IntLit(2)))), "each", None,
        Some(DoBlock(Some(ActualArgs(List(LVar("x")))), Stmnts(List(Binary(PLUS, LVar("a"), IntLit(1))))))
      )) { parse("""
[1,2].each do |x|
  a + 1
end
""")
      }
      assertResult(Cmd(Some(ConstLit("A")), "new", None,
        Some(DoBlock(None,Stmnts(List(Binary(PLUS, LVar("a"),IntLit(1)))))))) {
        parse("""
A.new do
 a + 1
end
""")
      }
    }
  }

  describe ("primary") {
    it("parses exclamation prefix") {
      assertResult(Unary(EXT, IVar("a"))) {
        parse("!@a")
      }
      assertResult(Unary(EXT, BoolLit(true))) {
        parse("!true")
      }
      assertResult(Unary(EXT, LVar("a"))) {
        parse("!a")
      }
      assertResult(Unary(EXT, ConstLit("A"))) {
        parse("!A")
      }
      assertResult(Unary(EXT, Cmd(Some(LVar("a")), "call", None, None))) {
        parse("!a.call")
      }
      assertResult(Unary(EXT, Binary(AND, BoolLit(true), BoolLit(false)))) {
        parse("!(true && false)")
      }
      assertResult(Binary(AND,Unary(EXT,BoolLit(true)),BoolLit(false))) {
        parse("!true && false")
      }
      assertResult(Unary(EXT, Unary(EXT, LVar("a")))) {
        parse("!!a")
      }
    }

    it ("parese minus prefix") {
      assertResult(Unary(MINUS, LVar("a"))) {
        parse("-a")
      }
      assertResult(Unary(MINUS, ConstLit("A"))) {
        parse("-A")
      }
      assertResult(Unary(MINUS, IntLit(10))) {
        parse("-10")
      }
      assertResult(Unary(MINUS, DoubleLit(10.0))){
        parse("-10.0")
      }
      assertResult(Unary(MINUS, Binary(PLUS, IntLit(1), IntLit(2)))) {
        parse("-(1+2)")
      }
    }

    it ("parses array ref") {
      assertResult(ARef(LVar("a"), IntLit(10))) {
        parse("a[10]")
      }
      assertResult(ARef(IVar("abc"), IntLit(10))) {
        parse("@abc[10]")
      }
      assertResult(ARef(IVar("abc"), LVar("i"))) {
        parse("@abc[i]")
      }
      assertResult(Cmd(Some(Ary(List(IntLit(1), IntLit(2)))), "each", None, None)) {
        parse("[1,2].each")
      }
    }

    it ("parses method call") {
      assertResult(Call(None, "call", Some(ActualArgs(List(LVar("a")))), None)) {
        parse("call(a)")
      }
      assertResult(Binary(PLUS, Call(None, "call", Some(ActualArgs(List(LVar("a")))), None), IntLit(1))) {
        parse("call(a) + 1")
      }
      assertResult(Call(Some(LVar("b")), "call", Some(ActualArgs(List(LVar("a")))), None)) {
        parse("b.call(a)")
      }
      assertResult(Call(Some(LVar("a1")), "call", Some(ActualArgs(List(IntLit(10), IntLit(10)))), None)) {
        parse("a1.call(10, 10)")
      }
      assertResult(Call(Some(LVar("a1")), "_call", Some(ActualArgs(List(IntLit(10)))), None)) {
        parse("a1._call(10)")
      }
      assertResult(Call(Some(LVar("a1")), "_calL?", Some(ActualArgs(List(IntLit(10)))), None)) {
        parse("a1._calL?(10)")
      }
      assertResult(Call(Some(LVar("a1")), "_calL!", Some(ActualArgs(List(IntLit(10)))), None)) {
        parse("a1._calL!(10)")
      }
      assertResult(Call(Some(LVar("a1")), "call", Some(ActualArgs(List())), None)) {
        parse("a1.call()")
      }
      assertResult(Binary(LT, Call(Some(LVar("a")), "call", Some(ActualArgs(Nil)), None), IntLit(10))) {
        parse("a.call() < 10")
      }
      assertResult(Call(Some(LVar("a1")), "call", Some(ActualArgs(List(IntLit(10), BoolLit(true)))), None)) {
        parse("a1.call(10, true)")
      }
    }

    it ("parses if expression") {
      assertResult(IfExpr(BoolLit(true), Stmnts(List(Binary(PLUS,IntLit(1),IntLit(2)))))) {
        parse("""
if  true
  1 + 2
end
""")
      }
      assertResult(IfExpr(Call(None, "a", Some(ActualArgs(List(IntLit(10)))), None), Stmnts(List(LVar("b"))))) {
        parse("""
if a(10)
  b
end
""")
      }
      assertResult(UnlessExpr(Call(None, "a", Some(ActualArgs(List(IntLit(10)))), None), Stmnts(List(LVar("b"))))) {
        parse("""
unless a(10)
  b
end
""")
      }
    }

    it ("parses class") {
      assertResult(ClassExpr(ConstLit("Sample"), Stmnts(Nil))) {
        parse("""
class Sample
end
""")
      }
      assertResult(ClassExpr(ConstLit("A"),
        Stmnts(List(DefExpr("a", None, Stmnts(List(Binary(PLUS, IntLit(1), IntLit(2))))))))) {
        parse("""
class A
  def a
    1 + 2
  end
end
""")
      }
    }

    it ("parses def") {
      assertResult(DefExpr("a", None, Stmnts(Nil))) {
        parse("""
def a
end
""")
      }
      assertResult(DefExpr("a?", None, Stmnts(Nil))) {
        parse("""
def a?
end
""")
      }
      assertResult(DefExpr("ASDF?", None, Stmnts(Nil))) {
        parse("""
def ASDF?
end
""")
      }
      assertResult(DefExpr("_a?", None, Stmnts(Nil))) {
        parse("""
def _a?
end
""")
      }
      assertResult(DefExpr("a?", Some(FormalArgs(Nil)), Stmnts(Nil))) {
        parse("""
def a?()
end
""")
      }
      assertResult(DefExpr("a", Some(FormalArgs(Nil)), Stmnts(Nil))) {
        parse("""
def a()
end
""")
      }
      assertResult(DefExpr("a", Some(FormalArgs(List(LVar("opt")))), Stmnts(Nil))) {
        parse("""
def a(opt)
end
""")
      }
      assertResult(DefExpr("call", Some(FormalArgs(List(LVar("a"), LVar("b")))), Stmnts(Nil))) {
        parse("""
def call(a, b)
end
""")
      }
      assertResult(DefExpr("call", None, Stmnts(List(StringLit(""""1+2""""))))) {
        parse("""
def call
 "1+2"
end
""")
      }
      assertResult(DefExpr("value=", Some(FormalArgs(List(LVar("v")))), Stmnts(List(Assign(IVar("value"), LVar("v"), EQ))))) {
        parse("""
def value=(v)
  @value = v
end""")
      }
    }
  }

  describe ("arg") {
    it ("parses Binary") {
      assertResult(Binary(PLUS, IntLit(1), IntLit(2))) {
        parse("1 + 2")
      }
      assertResult(Binary(PLUS, IntLit(1), Unary(MINUS, IntLit(2)))) {
        parse("1 + -2")
      }
      assertResult(Binary(PLUS, Unary(MINUS, IntLit(2)), IntLit(1))) {
        parse("-2 + 1")
      }
      assertResult(Binary(PLUS, IntLit(1), Binary(PLUS, IntLit(2), IntLit(2)))) {
        parse("1 + (2 + 2)")
      }
      assertResult(Binary(MINUS, IntLit(1), IntLit(2))) {
        parse("1 - 2")
      }
      assertResult(Binary(MUL, IntLit(1), IntLit(2))) {
        parse("1 * 2")
      }
      assertResult(Binary(DIV, IntLit(1), IntLit(2))) {
        parse("1 / 2")
      }
      assertResult(Binary(DIV, LVar("i"), IntLit(2))) {
        parse("i / 2")
      }
      assertResult(Binary(DIV, IVar("a"), IntLit(2))) {
        parse("@a / 2")
      }
      assertResult(Binary(MINUS, IntLit(1), Binary(MUL, IntLit(2), IntLit(3)))) {
        parse("1 - 2 * 3")
      }
      assertResult(Binary(MINUS, Binary(MINUS, IntLit(1), IntLit(2)), IntLit(3))) {
        parse("1 - 2 - 3")
      }
      assertResult(Binary(PLUS, Binary(PLUS, IntLit(1), Binary(MUL, IntLit(2), IntLit(1))), IntLit(10))) {
        parse("1 + 2 * 1 + 10")
      }
    }

    it ("parses compare binary") {
      assertResult(Binary(LT, IntLit(1), IntLit(2))) {
        parse("1 < 2")
      }
      assertResult(Binary(GE, IntLit(1), IntLit(2))) {
        parse("1 >= 2")
      }
      assertResult(Binary(GE, Binary(PLUS, IntLit(1), IntLit(2)), IntLit(3))) {
        parse("1 + 2 >= 3")
      }
    }

    it ("pares cond binary") {
      assertResult(Binary(AND, BoolLit(true), BoolLit(false))) {
        parse("true && false")
      }
      assertResult(Binary(OR, BoolLit(true), Binary(AND, BoolLit(false), BoolLit(false)))) {
        parse("true || false && false")
      }
    }
  }

  describe ("comment") {
    it ("ignore the code of following '#' ") {
      assertResult(DefExpr("call", None, Stmnts(List(StringLit(""""1+2""""))))) {
        parse("""
def call
 "1+2" # shoud be ingore
end
""")
      }
    }
  }

  describe ("Extendable") {
    it ("pares operator_with") {
      assertResult(Operators(
        List(Operator(Set("mod", "origin"),
          Syntax(Map("x" -> LVar("origin"), "y" -> LVar("origin")), List("x", "->", "y")),
          Stmnts(List(Assign(LVar("y"), LVar("x"), EQ))))))) {
        parse("""
Operator(mod, origin)
  defs x -> y ( x: origin, y: origin)
    y = x
  end
end
""")
      }
    }

    describe ("when mutiple defition in operator_with") {
      it ("parses(but this example is invalid)") {
        assertResult(Operators(List(
          Operator(Set("mod", "origin"),
            Syntax(Map("x" -> LVar("origin"), "y" -> LVar("origin")), List("x", "->", "y")),
            Stmnts(List(Assign(LVar("y"), LVar("x"), EQ)))),
          Operator(Set("mod", "origin"),
            Syntax(Map("x" -> LVar("origin"), "y" -> LVar("origin")), List("x", "<-", "y")),
            Stmnts(List(Assign(LVar("x"), LVar("y"), EQ))))))) {
          parse("""
Operator(mod, origin)
  defs x -> y ( x: origin, y: origin)
    y = x
  end

  defs x <- y ( x: origin, y: origin)
    x = y
  end
end
""")
        }
      }
    }

    describe ("when tag has condition") {
      it ("parses and, or (but this example is invalid)") {
        assertResult(Operators(List(
          Operator(Set("origin"), Syntax(Map("x" -> Binary(OR, LVar("origin"), LVar("mod"))), List("x", "<-", "1")), Stmnts(List(Assign(LVar("x"), IntLit(1), EQ)))),
          Operator(Set("origin"), Syntax(Map("x" -> Binary(OR, LVar("origin"), Binary(AND, LVar("origin"), LVar("mod"))), "y" -> Binary(AND, LVar("origin"), LVar("mod"))), List("x", "<-", "y")), Stmnts(List(Assign(LVar("x"), LVar("y"), EQ)))
        )))) {
          parse("""
Operator(mod, origin)
  defs x <- 1 (x: origin)
    x = 1
  end
end

Operator(origin)
  defs x <- 1 (x: origin || mod)
    x = 1
  end

  defs x <- y ( x: origin || (origin && mod), y: origin && mod )
    x = y
  end
end
""")
        }
      }
      it ("parses Not(!)") {
        assertResult(Call(None, "Operator::Origin::op_resources_Name", Some(ActualArgs(List(Call(None, "Operator::Resource_name::op_aws", None, None)))), None)) {
          parse("""
Operator(resource_name)
  defs aws()
    "aws"
  end
end

Operator(name)
  defs foo()
    "foo"
  end
end

Operator(origin)
  defs resources name (name: !name && !origin)
    resources name
  end
end

resources aws
""")
        }
      }
    }

    describe ("when using @token") {
      it ("parses @token") {
        assertResult(
          Call(None, "Operator::ModOrigin::op_X_4562",Some(ActualArgs(
            List(Call(None, "Operator::Foo::op_fn_A_94",Some(ActualArgs(
              List(Call(None, "Operator::Baz::op_fn_B_94",Some(ActualArgs(
                List(IntLit(10)))), None)))), None)))), None))
        {
          parse("""
Operator(foo)
 defs fn a ^(a: baz)
    a
 end
end

Operator(mod, origin)
  defs x -> (x: foo && @token(fn))
    x
  end
end

Operator(baz)
  defs fn b ^(b: origin)
    b
  end
end

fn fn 10 ^ ^ ->
""")
        }
      }

      it ("fail to parses @token") {
        assertThrows[parser.ParserErrors$NoSuchParser] {
          parse("""
Operator(baz)
  defs fnn b ^(b: origin)
    b
  end
end

Operator(foo)
 defs fn a ^(a: baz)
    a
 end
end

Operator(mod, origin)
  defs x -> (x: foo && @token(fn))
    x
  end
end

fn fnn 10 ^ ^ ->
""" )
        }
      }

      it ("parses !@token") {
        assertResult(
          Operators(List(
            Operator(Set("origin"),
              Syntax(Map("x" -> Binary(AND, LVar("origin"), Unary(EXT, ATToken("fn")))), List("fn", "x")),
              Stmnts(List(LVar("x")))))))
        {
          parse("""
Operator(origin)
  defs fn x(x: origin && !@token(fn))
    x
  end
end
""")
        }
      }
    }
  }

  def parse(in: String): Expr = (new Parser).parse(in + "\n") match {
    case Right(Stmnts(x)) => x.last
    case Left(s) => LVar(s)
  }
}
