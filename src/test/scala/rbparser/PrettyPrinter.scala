package rbparser

import org.scalatest._

class PrettyPrinterTest extends FunSpec {
  describe("literal") {
    it ("prints number") {
      assert("1" == pp(IntLit(1)))
      assert("1.2" == pp(DoubleLit(1.2)))
    }

    it ("prints id") {
      assert("x" == pp(LVar("x")))
      assert("_x" == pp(LVar("_x")))
    }

    it ("prints stringLit") {
      assert(""""asdf"""" == pp(StringLit("\"asdf\"")))
      assert(""""as\"df"""" == pp(StringLit("\"as\\\"df\"")))
      assert(""""as\ndf\n"""" == pp(StringLit("\"as\\ndf\\n\"")))
    }

    it ("prints bool") {
      assert("true" == pp(BoolLit(true)))
      assert("false" == pp(BoolLit(false)))
    }

    it ("prints const") {
      assert("ASDF" == pp(ConstLit("ASDF")))
      assert("ASDF2" == pp(ConstLit("ASDF2")))
    }

    it ("prints symbol") {
      assert(":asdf" == pp(SymbolLit("asdf")))
      assert(":_asdf" == pp(SymbolLit("_asdf")))
    }

    it ("prints instance varible") {
      assert("@a" == pp(IVar("a")))
      assert("@a1" == pp(IVar("a1")))
      assert("@_a1" == pp(IVar("_a1")))
    }

    it ("prints array") {
      assert("[]" == pp(Ary(Nil)))
      assert("[1]" == pp(Ary(List(IntLit(1)))))
      assert("""["asf", 2]""" == pp(Ary(List(StringLit(""""asf""""), IntLit(2)))))
    }

    it ("prints hash") {
      assert("{}" == pp(Hash(Map.empty)))
      assert("{ :key => 1 }" == pp(Hash(Map(SymbolLit("key") -> IntLit(1)))))
      assert("""{ "key1" => 1, :key2 => 2 }""" == pp(Hash(Map(StringLit("\"key1\"") -> IntLit(1), SymbolLit("key2") -> IntLit(2)))))
    }
  }

  describe("stmnt") {
    it ("prints simple assign") {
      assert("a = 1 + 2" == pp(Assign(LVar("a"), Binary(PLUS(), IntLit(1), IntLit(2)), EQ())))
      assert("@a = :keyword" == pp(Assign(IVar("a"), SymbolLit("keyword"), EQ())))
    }

    it ("prints assings stmnt") {
      assert("a = @a.call" == pp(Assign(LVar("a"), Cmd(Some(IVar("a")), "call", None, None), EQ())))
      assert("@a.size = 10" == pp(Assign(Cmd(Some(IVar("a")) ,"size", None, None), IntLit(10), EQ())))
      assert("@a[i] = 10" == pp(Assign(ARef(IVar("a"), LVar("i")), IntLit(10), EQ())))
      assert("@a ||= 1 + 2" == pp(Assign(IVar("a"), Binary(PLUS(), IntLit(1), IntLit(2)), ORE())))
    }

    it ("prints if modifier and unelss mod") {
      assert("10 if true" == pp(IfModExpr(BoolLit(true), IntLit(10))))
      assert("return 10 unless true" == pp(UnlessModExpr(BoolLit(true), Return(List(IntLit(10))))))
    }
  }

  describe("expr") {
    it ("prints return stmnt") {
      assert("return" == pp(Return(Nil)))
      assert("return a" == pp(Return(List(LVar("a")))))
      assert("return a, 10" == pp(Return(List(LVar("a"), IntLit(10)))))
    }

    it ("prints command call") {
      assert("A.new" == pp(Cmd(Some(ConstLit("A")), "new", None, None)))
      assert("@a.call" == pp(Cmd(Some(IVar("a")), "call", None, None)))
      assert("@a.call 10, 20" == pp(Cmd(Some(IVar("a")), "call", Some(ActualArgs(List(IntLit(10), IntLit(20)))), None)))
      assert("attr_reader :a, :b" == pp(Cmd(None, "attr_reader", Some(ActualArgs(List(SymbolLit("a"), SymbolLit("b")))), None)))
    }

    it ("prints method call with { ~ } block") {
      assert("""call(10) { |x| x + 1 }""" == pp(
        Call(
          None,
          "call",
          Some(ActualArgs(List(IntLit(10)))),
          Some(BraceBlock(Some(ActualArgs(List(LVar("x")))),
            Stmnts(List(Binary(PLUS(),LVar("x"),IntLit(1)))))))))

      assert("""a.call(10, 11) { |x| x + 1 }""" == pp(
        Call(
          Some(LVar("a")),
          "call",
          Some(ActualArgs(List(IntLit(10), IntLit(11)))),
          Some(BraceBlock(Some(ActualArgs(List(LVar("x")))),
            Stmnts(List(Binary(PLUS(),LVar("x"),IntLit(1)))))))))
    }

    it ("prints method call with do ~ end block") {
      assert("""call(10, :symbol) do |x|
  x + 1
end""" == pp(
        Call(
          None,
          "call",
          Some(ActualArgs(List(IntLit(10), SymbolLit("symbol")))),
          Some(DoBlock(Some(ActualArgs(List(LVar("x")))),
            Stmnts(List(Binary(PLUS(),LVar("x"), IntLit(1))))))
        )))

      assert("""a.call(10) do |x|
  x + 1
end""" == pp(
        Call(
          Some(LVar("a")),
          "call",
          Some(ActualArgs(List(IntLit(10)))),
          Some(DoBlock(Some(ActualArgs(List(LVar("x")))),
            Stmnts(List(Binary(PLUS(),LVar("x"), IntLit(1))))))
        )))
    }

    it ("prints command call with do ~ end block") {
      assert("""call :symbol do |x|
  x + 1
end""" == pp(
        Cmd(
          None,
          "call",
          Some(ActualArgs(List(SymbolLit("symbol")))),
          Some(DoBlock(Some(ActualArgs(List(LVar("x")))),
            Stmnts(List(Binary(PLUS(),LVar("x"), IntLit(1))))))
        )))

      assert("""call do |x|
  x + 1
end""" == pp(
        Cmd(
          None,
          "call",
          None,
          Some(DoBlock(Some(ActualArgs(List(LVar("x")))),
            Stmnts(List(Binary(PLUS(),LVar("x"), IntLit(1))))))
        )))

      assert("""[1, 2].each do |x|
  x + 1
end""" == pp(
        Cmd(
          Some(Ary(List(IntLit(1), IntLit(2)))),
          "each",
          None,
          Some(
            DoBlock(Some(ActualArgs(List(LVar("x")))),
              Stmnts(List(Binary(PLUS(), LVar("x"), IntLit(1))))))
        )))
    }

    it ("prints command call with { ~ } block") {
      assert("""call { |x| x + 1 }""" == pp(
        Cmd(
          None,
          "call",
          None,
          Some(BraceBlock(Some(ActualArgs(List(LVar("x")))),
            Stmnts(List(Binary(PLUS(),LVar("x"),IntLit(1)))))))))

      assert("""call :aaa { |x| x + 1 }""" == pp(
        Cmd(
          None,
          "call",
          Some(ActualArgs(List(SymbolLit("aaa")))),
          Some(BraceBlock(Some(ActualArgs(List(LVar("x")))),
            Stmnts(List(Binary(PLUS(),LVar("x"),IntLit(1)))))))))
    }
  }

  describe("primary") {
    it ("prints exclamation prefix") {
      assert("!@a" == pp(Unary(EXT(), IVar("a"))))
      assert("!true" == pp(Unary(EXT(), BoolLit(true))))
      assert("!A" == pp(Unary(EXT(), ConstLit("A"))))
      assert("!a.call" == pp(Unary(EXT(), Cmd(Some(LVar("a")), "call", None, None))))
      assert("!(true && false)" == pp(Unary(EXT(), Binary(AND(), BoolLit(true), BoolLit(false)))))
      assert("!true && false" == pp(Binary(AND(),Unary(EXT(),BoolLit(true)),BoolLit(false))))
      assert("!!a" == pp(Unary(EXT(), Unary(EXT(), LVar("a")))))
    }

    it ("prints minus prefix") {
      assert("-10" == pp(Unary(MINUS(), IntLit(10))))
      assert("-a" == pp(Unary(MINUS(), LVar("a"))))
      assert("-A" == pp(Unary(MINUS(), ConstLit("A"))))
      assert("-(1 + 2)" == pp(Unary(MINUS(), Binary(PLUS(), IntLit(1), IntLit(2)))))
    }

    it ("prints array ref") {
      assert("a[10]" == pp(ARef(LVar("a"), IntLit(10))))
      assert("@a[10]" == pp(ARef(IVar("a"), IntLit(10))))
      assert("@a[i]" == pp(ARef(IVar("a"), LVar("i"))))
      assert("[1, 2].each" == pp(Cmd(Some(Ary(List(IntLit(1), IntLit(2)))), "each", None, None)))
    }

    it ("prints method call") {
      assert("call(a)" == pp(Call(None, "call", Some(ActualArgs(List(LVar("a")))), None)))
      assert("call(a) + 1" == pp(Binary(PLUS(), Call(None, "call", Some(ActualArgs(List(LVar("a")))), None), IntLit(1))))
      assert("a1.call" == pp(Call(Some(LVar("a1")), "call", None, None)))
      assert("a1.call(10, true)" == pp(Call(Some(LVar("a1")), "call", Some(ActualArgs(List(IntLit(10), BoolLit(true)))), None)))
    }

    it ("prints if expression") {
      assert("""if true
  1 + 2
end""" == pp(IfExpr(BoolLit(true), Stmnts(List(Binary(PLUS(),IntLit(1),IntLit(2)))))))

      assert("""unless a(10)
  b
end""" == pp(UnlessExpr(Call(None, "a", Some(ActualArgs(List(IntLit(10)))), None), Stmnts(List(LVar("b"))))))
    }

    it ("print class expr") {
      assert("""class A
end""" == pp(ClassExpr(ConstLit("A"), Stmnts(List()))))

      assert("""class A
  def a
    1 + 2
  end
end""" == pp(
        ClassExpr(ConstLit("A"),
          Stmnts(List(
            DefExpr(("a"), None, Stmnts(List(Binary(PLUS(), IntLit(1), IntLit(2)))))))
        )))
    }

    it ("print def expr") {
      assert("""def a
end""" == pp(DefExpr(("a"), None, Stmnts(List()))))

      assert("""def a(opt)
end""" == pp(DefExpr(("a"), Some(FormalArgs(List(LVar("opt")))), Stmnts(List()))))
    }
 }

  describe("arg") {
    it ("prints binary expr") {
      assert("1 + 2" == pp(Binary(PLUS(), IntLit(1), IntLit(2))))
      assert("-2 + 1" == pp(Binary(PLUS(), Unary(MINUS(), IntLit(2)), IntLit(1))))
      assert("1 * 2" == pp(Binary(AST(), IntLit(1), IntLit(2))))
      assert("1 + 2 * 1 + 10" == pp(Binary(PLUS(), Binary(PLUS(), IntLit(1), Binary(AST(), IntLit(2), IntLit(1))), IntLit(10))))
      assert("1 - 2 - 3" == pp(Binary(MINUS(), Binary(MINUS(), IntLit(1), IntLit(2)), IntLit(3))))
    }

    it ("prints compare binary") {
      assert("1 < 2" == pp(Binary(LT(), IntLit(1), IntLit(2))))
      assert("1 >= 2" == pp(Binary(GE(), IntLit(1), IntLit(2))))
      assert("1 + 2 >= 3" == pp(Binary(GE(), Binary(PLUS(), IntLit(1), IntLit(2)), IntLit(3))))
    }

    it ("prints cond binary") {
      assert("true && false" == pp(Binary(AND(), BoolLit(true), BoolLit(false))))
      assert("true || false && false" == pp(Binary(OR(), BoolLit(true), Binary(AND(), BoolLit(false), BoolLit(false)))))
    }
  }

  def pp(ast: ASTs): String = PrettyPrinter.call(ast)
}
