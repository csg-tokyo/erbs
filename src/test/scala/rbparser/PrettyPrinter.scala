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
      assert(pp(Ary(None)) == "[]")
      assert(pp(Ary(Some(List(IntLit(1))))) == "[1]")
      assert(pp(Ary(Some(List(StringLit(""""asf""""), IntLit(2))))) == """["asf", 2]""")
    }
  }

  describe("stmnt") {
    it ("prints simple assign") {
      assert(pp(Assign(LVar("a"), Binary(PLUS(), IntLit(1), IntLit(2)))) == "a = 1 + 2")
      assert(pp(Assign(IVar("a"),SymbolLit("keyword"))) == "@a = :keyword")
    }

    it ("prints assings stmnt") {
      assert(pp(Assign(LVar("a"), Call(Some(IVar("a")), MethodName("call"), None, None))) == "a = @a.call")
      assert(pp(Assign(Call(Some(IVar("a")) ,MethodName("size"), None, None), IntLit(10))) == "@a.size = 10")
      assert(pp(Assign(ARef(IVar("a"), LVar("i")), IntLit(10))) == "@a[i] = 10")
    }

    // it ("parses post modifier") {
    //   parse("10 if true") { v => assert(v == IfExpr(BoolLit(true), Stmnts(List(IntLit(10))))) }
    //   parse("return 10 if true") { v => assert(v == IfExpr(BoolLit(true), Stmnts(List(Return(List(IntLit(10))))))) }
    //   parse("a[i] unless a.hash?") { v => assert(v == UnlessExpr(Call(Some(LVar("a")), MethodName("hash?"), None, None), Stmnts(List(ARef(LVar("a"), LVar("i")))))) }
    // }
  }

  def pp(ast: ASTs): String = PrettyPrinter.call(ast)
}
