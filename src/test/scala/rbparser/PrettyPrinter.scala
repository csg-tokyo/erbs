package rbparser

import org.scalatest._

class PrettyPrinterTest extends FunSpec {
  describe("literal") {
    it ("prints number") {
      assert(pp(IntLit(1)) == "1")
      assert(pp(DoubleLit(1.2)) == "1.2")
    }

    it ("prints id") {
      assert(pp(LVar("x")) == "x")
      assert(pp(LVar("_x")) == "_x")
    }

    it ("prints stringLit") {
      assert(pp(StringLit("\"asdf\"")) == """"asdf"""")
      assert(pp(StringLit("\"as\\\"df\"")) == """"as\"df"""")
      assert(pp(StringLit("\"as\\ndf\\n\"")) == """"as\ndf\n"""")
    }

    it ("prints bool") {
      assert(pp(BoolLit(true)) == "true")
      assert(pp(BoolLit(false)) == "false")
    }

    it ("prints const") {
      assert(pp(ConstLit("ASDF")) == "ASDF")
      assert(pp(ConstLit("ASDF2")) == "ASDF2")
    }

    it ("prints symbol") {
      assert(pp(SymbolLit("asdf")) == ":asdf")
      assert(pp(SymbolLit("_asdf")) == ":_asdf")
    }

    it ("prints instance varible") {
      assert(pp(IVar("a")) == "@a")
      assert(pp(IVar("a1")) == "@a1")
      assert(pp(IVar("_a1")) == "@_a1")
    }

    it ("prints array") {
      assert(pp(Ary(Nil)) == "[]")
      assert(pp(Ary(List(IntLit(1)))) == "[1]")
      assert(pp(Ary(List(StringLit(""""asf""""), IntLit(2)))) == """["asf", 2]""")
    }
  }

  describe("stmnt") {
    it ("prints simple assign") {
      assert(pp(Assign(LVar("a"), Binary(PLUS(), IntLit(1), IntLit(2)), EQ())) == "a = 1 + 2")
      assert(pp(Assign(IVar("a"), SymbolLit("keyword"), EQ())) == "@a = :keyword")
    }

    it ("prints assings stmnt") {
      assert(pp(Assign(LVar("a"), Cmd(Some(IVar("a")), "call", None, None), EQ())) == "a = @a.call")
      assert(pp(Assign(Cmd(Some(IVar("a")) ,"size", None, None), IntLit(10), EQ())) == "@a.size = 10")
      assert(pp(Assign(ARef(IVar("a"), LVar("i")), IntLit(10), EQ())) == "@a[i] = 10")
      assert(pp(Assign(IVar("a"), Binary(PLUS(), IntLit(1), IntLit(2)), ORE())) == "@a ||= 1 + 2")
    }

    it ("parses if modifier and unelss moe") {
      assert(pp(IfModExpr(BoolLit(true), IntLit(10))) == "10 if true")
      assert(pp(UnlessModExpr(BoolLit(true), Return(List(IntLit(10))))) == "return 10 unless true")
    }
  }

  describe("expr") {
    it ("prints return stmnt") {
      assert(pp(Return(Nil)) == "return")
      assert(pp(Return(List(LVar("a")))) == "return a")
      assert(pp(Return(List(LVar("a"), IntLit(10)))) == "return a, 10")
    }

    it ("prints command call") {
      assert(pp(Cmd(Some(ConstLit("A")), "new", None, None)) == "A.new")
      assert(pp(Cmd(Some(IVar("a")), "call", None, None)) == "@a.call")
      assert(pp(Cmd(Some(IVar("a")), "call", Some(ActualArgs(List(IntLit(10), IntLit(20)))), None)) == "@a.call 10, 20")
      assert(pp(Cmd(None, "attr_reader", Some(ActualArgs(List(SymbolLit("a"), SymbolLit("b")))), None)) == "attr_reader :a, :b")
    }

    it ("parses method call with { ~ } block") {
      assert(pp(
        Call(
          None,
          "call",
          Some(ActualArgs(List(IntLit(10)))),
          Some(BraceBlock(Some(ActualArgs(List(LVar("x")))),
            Stmnts(List(Binary(PLUS(),LVar("x"),IntLit(1)))))))) == """call(10) { |x| x + 1 }""")

      assert(pp(
        Call(
          Some(LVar("a")),
          "call",
          Some(ActualArgs(List(IntLit(10), IntLit(11)))),
          Some(BraceBlock(Some(ActualArgs(List(LVar("x")))),
            Stmnts(List(Binary(PLUS(),LVar("x"),IntLit(1)))))))) == """a.call(10, 11) { |x| x + 1 }""")
    }

    it ("prints method call with do ~ end block") {
      assert(pp(
        Call(
          None,
          "call",
          Some(ActualArgs(List(IntLit(10), SymbolLit("symbol")))),
          Some(DoBlock(Some(ActualArgs(List(LVar("x")))),
            Stmnts(List(Binary(PLUS(),LVar("x"), IntLit(1))))))
        )) ==
        """call(10, :symbol) do |x|
x + 1
end"""
      )

      assert(pp(
        Call(
          Some(LVar("a")),
          "call",
          Some(ActualArgs(List(IntLit(10)))),
          Some(DoBlock(Some(ActualArgs(List(LVar("x")))),
            Stmnts(List(Binary(PLUS(),LVar("x"), IntLit(1))))))
        )) ==
        """a.call(10) do |x|
x + 1
end"""
      )
    }

    it ("prints command call with do ~ end block") {
      assert(pp(
        Cmd(
          None,
          "call",
          Some(ActualArgs(List(SymbolLit("symbol")))),
          Some(DoBlock(Some(ActualArgs(List(LVar("x")))),
            Stmnts(List(Binary(PLUS(),LVar("x"), IntLit(1))))))
        )) ==
        """call :symbol do |x|
x + 1
end"""
      )

      assert(pp(
        Cmd(
          None,
          "call",
          None,
          Some(DoBlock(Some(ActualArgs(List(LVar("x")))),
            Stmnts(List(Binary(PLUS(),LVar("x"), IntLit(1))))))
        )) ==
        """call do |x|
x + 1
end"""
      )

      assert(pp(
        Cmd(
          Some(Ary(List(IntLit(1), IntLit(2)))),
          "each",
          None,
          Some(
            DoBlock(Some(ActualArgs(List(LVar("x")))),
              Stmnts(List(Binary(PLUS(), LVar("x"), IntLit(1))))))
        )) ==
        """[1, 2].each do |x|
x + 1
end"""
      )
    }

    it ("prints command call with { ~ } block") {
      assert(pp(
        Cmd(
          None,
          "call",
          None,
          Some(BraceBlock(Some(ActualArgs(List(LVar("x")))),
            Stmnts(List(Binary(PLUS(),LVar("x"),IntLit(1)))))))) == """call { |x| x + 1 }""")

      assert(pp(
        Cmd(
          None,
          "call",
          Some(ActualArgs(List(SymbolLit("aaa")))),
          Some(BraceBlock(Some(ActualArgs(List(LVar("x")))),
            Stmnts(List(Binary(PLUS(),LVar("x"),IntLit(1)))))))) == """call :aaa { |x| x + 1 }""")
    }
  }

  describe("primary") {
    it ("prints exclamation prefix") {
      assert(pp(Unary(EXT(), IVar("a"))) == "!@a")
      assert(pp(Unary(EXT(), BoolLit(true))) == "!true")
      assert(pp(Unary(EXT(), ConstLit("A"))) == "!A")
      assert(pp(Unary(EXT(), Cmd(Some(LVar("a")), "call", None, None))) == "!a.call")
      assert(pp(Unary(EXT(), Binary(AND(), BoolLit(true), BoolLit(false)))) == "!(true && false)")
      assert(pp(Binary(AND(),Unary(EXT(),BoolLit(true)),BoolLit(false))) == "!true && false")
      assert(pp(Unary(EXT(), Unary(EXT(), LVar("a")))) == "!!a")
    }

    it ("prints minus prefix") {
      assert(pp(Unary(MINUS(), IntLit(10))) == "-10")
      assert(pp(Unary(MINUS(), LVar("a"))) == "-a")
      assert(pp(Unary(MINUS(), ConstLit("A"))) == "-A")
      assert(pp(Unary(MINUS(), Binary(PLUS(), IntLit(1), IntLit(2)))) == "-(1 + 2)")
    }

    it ("prints array ref") {
      assert(pp(ARef(LVar("a"), IntLit(10))) == "a[10]")
      assert(pp(ARef(IVar("a"), IntLit(10))) == "@a[10]")
      assert(pp(ARef(IVar("a"), LVar("i"))) == "@a[i]")
      assert(pp(Cmd(Some(Ary(List(IntLit(1), IntLit(2)))), "each", None, None)) == "[1, 2].each")
    }

    it ("prints method call") {
      assert(pp(Call(None, "call", Some(ActualArgs(List(LVar("a")))), None)) == "call(a)")
      assert(pp(Binary(PLUS(), Call(None, "call", Some(ActualArgs(List(LVar("a")))), None), IntLit(1))) == "call(a) + 1")
      assert(pp(Call(Some(LVar("a1")), "call", None, None)) == "a1.call")
      assert(pp(Call(Some(LVar("a1")), "call", Some(ActualArgs(List(IntLit(10), BoolLit(true)))), None)) == "a1.call(10, true)")
    }

    it ("prints if expression") {
      assert(pp(IfExpr(BoolLit(true), Stmnts(List(Binary(PLUS(),IntLit(1),IntLit(2))))))
        == """if true
1 + 2
end"""
      )

      assert(pp(UnlessExpr(Call(None, "a", Some(ActualArgs(List(IntLit(10)))), None), Stmnts(List(LVar("b")))))
        == """unless a(10)
b
end"""
      )
    }

    it ("print class expr") {
      assert(pp(ClassExpr(ConstLit("A"), Stmnts(List())))
        == """class A
end"""
      )

      assert(pp(
        ClassExpr(ConstLit("A"),
          Stmnts(List(
            DefExpr(("a"), None, Stmnts(List(Binary(PLUS(), IntLit(1), IntLit(2)))))))
        )) == """class A
def a
1 + 2
end
end""")
    }

    it ("print def expr") {
      assert(pp(DefExpr(("a"), None, Stmnts(List()))) == """def a
end""")

      assert(pp(DefExpr(("a"), Some(FormalArgs(List(LVar("opt")))), Stmnts(List()))) == """def a(opt)
end""")
    }
  }

  describe("arg") {
    it ("prints binary expr") {
      assert(pp(Binary(PLUS(), IntLit(1), IntLit(2))) == "1 + 2")
      assert(pp(Binary(PLUS(), Unary(MINUS(), IntLit(2)), IntLit(1))) == "-2 + 1")
      assert(pp(Binary(AST(), IntLit(1), IntLit(2))) == "1 * 2")
      assert(pp(Binary(PLUS(), Binary(PLUS(), IntLit(1), Binary(AST(), IntLit(2), IntLit(1))), IntLit(10))) == "1 + 2 * 1 + 10")
      assert(pp(Binary(MINUS(), Binary(MINUS(), IntLit(1), IntLit(2)), IntLit(3))) == "1 - 2 - 3")
    }

    it ("prints compare binary") {
      assert(pp(Binary(LT(), IntLit(1), IntLit(2))) == "1 < 2")
      assert(pp(Binary(GE(), IntLit(1), IntLit(2))) == "1 >= 2")
      assert(pp(Binary(GE(), Binary(PLUS(), IntLit(1), IntLit(2)), IntLit(3))) == "1 + 2 >= 3")
    }

    it ("prints cond binary") {
      assert(pp(Binary(AND(), BoolLit(true), BoolLit(false))) == "true && false")
      assert(pp(Binary(OR(), BoolLit(true), Binary(AND(), BoolLit(false), BoolLit(false)))) == "true || false && false")
    }
  }

  def pp(ast: ASTs): String = PrettyPrinter.call(ast)
}
