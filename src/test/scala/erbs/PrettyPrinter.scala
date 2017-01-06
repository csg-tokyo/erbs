package erbs

import erbs.parser.ast._

import org.scalatest._

class PrettyPrinterTest extends FunSpec {
  describe("literal") {
    it ("prints number") {
      assertResult("1") {
        pp(IntLit(1))
      }
      assertResult("1.2") {
        pp(DoubleLit(1.2))
      }
    }

    it ("prints id") {
      assertResult("x") {
        pp(LVar("x"))
      }
      assertResult("_x") {
        pp(LVar("_x"))
      }
    }

    it ("prints stringLit") {
      assertResult(""""asdf"""") {
        pp(StringLit("\"asdf\""))
      }
      assertResult(""""as\"df"""") {
        pp(StringLit("\"as\\\"df\""))
      }
      assertResult(""""as\ndf\n"""") {
        pp(StringLit("\"as\\ndf\\n\""))
      }
    }

    it ("prints bool") {
      assertResult("true") {
        pp(BoolLit(true))
      }
      assertResult("false") {
        pp(BoolLit(false))
      }
    }

    it ("prints const") {
      assertResult("ASDF") {
        pp(ConstLit("ASDF"))
      }
      assertResult("ASDF2") {
        pp(ConstLit("ASDF2"))
      }
      assertResult("A::B") {
        pp(ConstLit("A::B"))
      }
    }

    it ("prints symbol") {
      assertResult(":asdf") {
        pp(SymbolLit("asdf"))
      }
      assertResult(":_asdf") {
        pp(SymbolLit("_asdf"))
      }
    }

    it ("prints instance varible") {
      assertResult("@a") {
        pp(IVar("a"))
      }
      assertResult("@a1") {
        pp(IVar("a1"))
      }
      assertResult("@_a1") {
        pp(IVar("_a1"))
      }
    }

    it ("prints array") {
      assertResult("[]") {
        pp(Ary(Nil))
      }
      assertResult("[1]") {
        pp(Ary(List(IntLit(1))))
      }
      assertResult("""["asf", 2]""") {
        pp(Ary(List(StringLit(""""asf""""), IntLit(2))))
      }
    }

    it ("prints hash") {
      assertResult("{}") {
        pp(Hash(Map.empty))
      }
      assertResult("{ :key => 1 }") {
        pp(Hash(Map(SymbolLit("key") -> IntLit(1))))
      }
      assertResult("""{ "key1" => 1, :key2 => 2 }""") {
        pp(Hash(Map(StringLit("\"key1\"") -> IntLit(1), SymbolLit("key2") -> IntLit(2))))
      }
    }
  }

  describe("stmnt") {
    it ("prints simple assign") {
      assertResult("a = 1 + 2") {
        pp(Assign(LVar("a"), Binary(PLUS, IntLit(1), IntLit(2)), EQ))
      }
      assertResult("@a = :keyword") {
        pp(Assign(IVar("a"), SymbolLit("keyword"), EQ))
      }
    }

    it ("prints assings stmnt") {
      assertResult("a = @a.call") {
        pp(Assign(LVar("a"), Cmd(Some(IVar("a")), "call", None, None), EQ))
      }
      assertResult("@a.size = 10") {
        pp(Assign(Cmd(Some(IVar("a")) ,"size", None, None), IntLit(10), EQ))
      }
      assertResult("@a[i] = 10") {
        pp(Assign(ARef(IVar("a"), LVar("i")), IntLit(10), EQ))
      }
      assertResult("@a ||= 1 + 2") {
        pp(Assign(IVar("a"), Binary(PLUS, IntLit(1), IntLit(2)), ORE))
      }
    }

    it ("prints if modifier and unelss mod") {
      assertResult("10 if true") {
        pp(IfModExpr(BoolLit(true), IntLit(10)))
      }
      assertResult("return 10 unless true") {
        pp(UnlessModExpr(BoolLit(true), Return(List(IntLit(10)))))
      }
    }
  }

  describe("expr") {
    it ("prints return stmnt") {
      assertResult("return") {
        pp(Return(Nil))
      }
      assertResult("return a") {
        pp(Return(List(LVar("a"))))
      }
      assertResult("return a, 10") {
        pp(Return(List(LVar("a"), IntLit(10))))
      }
    }

    it ("prints command call") {
      assertResult("A.new") {
        pp(Cmd(Some(ConstLit("A")), "new", None, None))
      }
      assertResult("@a.call") {
        pp(Cmd(Some(IVar("a")), "call", None, None))
      }
      assertResult("@a.call 10, 20") {
        pp(Cmd(Some(IVar("a")), "call", Some(ActualArgs(List(ActualArgElement(IntLit(10)), ActualArgElement(IntLit(20))))), None))
      }
      assertResult("attr_reader :a, :b") {
        pp(Cmd(None, "attr_reader", Some(ActualArgs(List(ActualArgElement(SymbolLit("a")), ActualArgElement(SymbolLit("b"))))), None))
      }
    }

    it ("prints method call with { ~ } block") {
      assertResult("""call(10) { |x| x + 1 }""") {
        pp(Call(None, "call",
          Some(ActualArgs(List(ActualArgElement(IntLit(10))))),
          Some(BraceBlock(Some(ActualArgs(List(ActualArgElement(LVar("x"))))), Stmnts(List(Binary(PLUS,LVar("x"),IntLit(1))))))))
      }
      assertResult("""a.call(10, 11) { |x| x + 1 }""") {
        pp(Call(Some(LVar("a")), "call",
          Some(ActualArgs(List(ActualArgElement(IntLit(10)), ActualArgElement(IntLit(11))))),
          Some(BraceBlock(Some(ActualArgs(List(ActualArgElement(LVar("x"))))), Stmnts(List(Binary(PLUS,LVar("x"),IntLit(1))))))))
      }
    }

    it ("prints method call with do ~ end block") {
      assertResult("""call(10, :symbol) do |x|
  x + 1
end""") {
        pp(
          Call(None, "call",
            Some(ActualArgs(List(ActualArgElement(IntLit(10)), ActualArgElement(SymbolLit("symbol"))))),
            Some(DoBlock(Some(ActualArgs(List(ActualArgElement(LVar("x"))))), Stmnts(List(Binary(PLUS,LVar("x"), IntLit(1))))))
          ))
      }
      assertResult("""a.call(10) do |x|
  x + 1
end""") {
        pp(Call(Some(LVar("a")), "call",
          Some(ActualArgs(List(ActualArgElement(IntLit(10))))),
          Some(DoBlock(Some(ActualArgs(List(ActualArgElement(LVar("x"))))), Stmnts(List(Binary(PLUS,LVar("x"), IntLit(1))))))
        ))
      }
    }

    it ("prints command call with do ~ end block") {
      assertResult("""call :symbol do |x|
  x + 1
end""") {
        pp(Cmd(None, "call",
          Some(ActualArgs(List(ActualArgElement(SymbolLit("symbol"))))),
          Some(DoBlock(Some(ActualArgs(List(ActualArgElement(LVar("x"))))), Stmnts(List(Binary(PLUS,LVar("x"), IntLit(1))))))
        ))
      }

      assertResult("""call do |x|
  x + 1
end""") {
        pp(Cmd(None, "call", None,
          Some(DoBlock(Some(ActualArgs(List(ActualArgElement(LVar("x"))))),
            Stmnts(List(Binary(PLUS,LVar("x"), IntLit(1))))))
        ))
      }

      assertResult("""[1, 2].each do |x|
  x + 1
end""") {
        pp(Cmd(Some(Ary(List(IntLit(1), IntLit(2)))), "each", None,
          Some(DoBlock(Some(ActualArgs(List(ActualArgElement(LVar("x"))))),
            Stmnts(List(Binary(PLUS, LVar("x"), IntLit(1))))))
        ))
      }
    }

    it ("prints command call with { ~ } block") {
      assertResult("""call { |x| x + 1 }""") {
        pp(Cmd(None, "call", None,
          Some(BraceBlock(Some(ActualArgs(List(ActualArgElement(LVar("x"))))),
            Stmnts(List(Binary(PLUS,LVar("x"),IntLit(1))))))))
      }

      assertResult("""call :aaa { |x| x + 1 }""") {
        pp(Cmd(None, "call",
          Some(ActualArgs(List(ActualArgElement(SymbolLit("aaa"))))),
          Some(BraceBlock(Some(ActualArgs(List(ActualArgElement(LVar("x"))))),
            Stmnts(List(Binary(PLUS,LVar("x"),IntLit(1))))))))
      }
    }

    describe("primary") {
      it ("prints exclamation prefix") {
        assertResult("!@a") {
          pp(Unary(EXT, IVar("a")))
        }
        assertResult("!true") {
          pp(Unary(EXT, BoolLit(true)))
        }
        assertResult("!A") {
          pp(Unary(EXT, ConstLit("A")))
        }
        assertResult("!a.call") {
          pp(Unary(EXT, Cmd(Some(LVar("a")), "call", None, None)))
        }
        assertResult("!(true && false)") {
          pp(Unary(EXT, Binary(AND, BoolLit(true), BoolLit(false))))
        }
        assertResult("!true && false") {
          pp(Binary(AND,Unary(EXT,BoolLit(true)),BoolLit(false)))
        }
        assertResult("!!a") {
          pp(Unary(EXT, Unary(EXT, LVar("a"))))
        }
      }

      it ("prints minus prefix") {
        assertResult("-10") {
          pp(Unary(MINUS, IntLit(10)))
        }
        assertResult("-a") {
          pp(Unary(MINUS, LVar("a")))
        }
        assertResult("-A") {
          pp(Unary(MINUS, ConstLit("A")))
        }
        assertResult("-(1 + 2)") {
          pp(Unary(MINUS, Binary(PLUS, IntLit(1), IntLit(2))))
        }
      }

      it ("prints array ref") {
        assertResult("a[10]") {
          pp(ARef(LVar("a"), IntLit(10)))
        }
        assertResult("@a[10]") {
          pp(ARef(IVar("a"), IntLit(10)))
        }
        assertResult("@a[i]") {
          pp(ARef(IVar("a"), LVar("i")))
        }
        assertResult("[1, 2].each") {
          pp(Cmd(Some(Ary(List(IntLit(1), IntLit(2)))), "each", None, None))
        }
      }

      it ("prints method call") {
        assertResult("call(a)") {
          pp(Call(None, "call", Some(ActualArgs(List(ActualArgElement(LVar("a"))))), None))
        }
        assertResult("call(a) + 1") {
          pp(Binary(PLUS, Call(None, "call", Some(ActualArgs(List(ActualArgElement(LVar("a"))))), None), IntLit(1)))
        }
        assertResult("a1.call") {
          pp(Call(Some(LVar("a1")), "call", None, None))
        }
        assertResult("a1.call(10, true)") {
          pp(Call(Some(LVar("a1")), "call", Some(ActualArgs(List(ActualArgElement(IntLit(10)), ActualArgElement(BoolLit(true))))), None))
        }
      }

      it ("prints if expression") {
        assertResult("""if true
  1 + 2
end""") { pp(IfExpr(BoolLit(true), Stmnts(List(Binary(PLUS,IntLit(1),IntLit(2)))), Nil, None)) }
        assertResult("""unless a(10)
  b
end""") { pp(UnlessExpr(Call(None, "a", Some(ActualArgs(List(ActualArgElement(IntLit(10))))), None), Stmnts(List(LVar("b"))), None)) }
        assertResult("""if a(10)
  b
else
  c
end""") { pp(IfExpr(Call(None, "a", Some(ActualArgs(List(ActualArgElement(IntLit(10))))), None), Stmnts(List(LVar("b"))), Nil, Some((Stmnts(List(LVar("c"))))))) }
        assertResult("""if a(10)
  b
elsif 1
  c
end""") { pp(IfExpr(Call(None, "a", Some(ActualArgs(List(ActualArgElement(IntLit(10))))), None), Stmnts(List(LVar("b"))), List(ElsifBody(IntLit(1), Stmnts(List(LVar("c"))))), None)) }
      }

      it ("print lass expr") {
        assertResult("""class A
end""") { pp(ClassExpr(ConstLit("A"), None, Stmnts(Nil))) }
        assertResult("""class A < B
end""") { pp(ClassExpr(ConstLit("A"), Some(ConstLit("B")), Stmnts(Nil))) }
        assertResult("""class A < B::C
end""") { pp(ClassExpr(ConstLit("A"), Some(ConstLit("B::C")), Stmnts(Nil))) }
        assertResult("""class A
  def a
    1 + 2
  end
end""") {
          pp(ClassExpr(ConstLit("A"), None, Stmnts(List(
            DefExpr(("a"), None, Stmnts(List(Binary(PLUS, IntLit(1), IntLit(2)))))))))
        }
        assertResult("""class A < B
  def a
    1 + 2
  end
end""") {
          pp(ClassExpr(ConstLit("A"), Some(ConstLit("B")), Stmnts(List(
            DefExpr(("a"), None, Stmnts(List(Binary(PLUS, IntLit(1), IntLit(2)))))))))
        }
      }

      it ("print def expr") {
        assertResult("""def a
end""") {
          pp(DefExpr(("a"), None, Stmnts(Nil)))
        }

        assertResult("""def a(opt)
end""") {
          pp(DefExpr(("a"), Some(FormalArgs(List(ActualArgElement(LVar("opt"))))), Stmnts(Nil)))
        }

        assertResult("""def call(key = 1)
end""") {
       pp(DefExpr("call", Some(FormalArgs(List(DefaultArgElement("key", IntLit(1))))), Stmnts(Nil)))
        }

      assertResult("""def call(key: 1)
end""") {
       pp(DefExpr("call", Some(FormalArgs(List(KeywordArgElement("key", IntLit(1))))), Stmnts(Nil)))
        }

      assertResult("""def call(v, key: 1)
end""") {
       pp(DefExpr("call", Some(FormalArgs(List(SimpleArgElement("v"), KeywordArgElement("key", IntLit(1))))), Stmnts(Nil)))
        }

      }
    }

    describe("arg") {
      it ("prints binary expr") {
        assertResult("1 + 2") {
          pp(Binary(PLUS, IntLit(1), IntLit(2)))
        }
        assertResult("-2 + 1") {
          pp(Binary(PLUS, Unary(MINUS, IntLit(2)), IntLit(1)))
        }
        assertResult("1 * 2") {
          pp(Binary(MUL, IntLit(1), IntLit(2)))
        }
        assertResult("1 + 2 * 1 + 10") {
          pp(Binary(PLUS, Binary(PLUS, IntLit(1), Binary(MUL, IntLit(2), IntLit(1))), IntLit(10)))
        }
        assertResult("1 - 2 - 3") {
          pp(Binary(MINUS, Binary(MINUS, IntLit(1), IntLit(2)), IntLit(3)))
        }
      }

      it ("prints compare binary") {
        assertResult("1 < 2") {
          pp(Binary(LT, IntLit(1), IntLit(2)))
        }
        assertResult("1 >= 2") {
          pp(Binary(GE, IntLit(1), IntLit(2)))
        }
        assertResult("1 + 2 >= 3") {
          pp(Binary(GE, Binary(PLUS, IntLit(1), IntLit(2)), IntLit(3)))
        }
      }

      it ("prints cond binary") {
        assertResult("true && false") {
          pp(Binary(AND, BoolLit(true), BoolLit(false)))
        }
      }
      assertResult("true || false && false") {
        pp(Binary(OR, BoolLit(true), Binary(AND, BoolLit(false), BoolLit(false))))
      }
    }

    describe("nested") {
      it ("prints valid intended code") {
        assertResult("""class A
  def a
    if true
      b
    else
      c
    end
  end
end""") { pp(ClassExpr(ConstLit("A"), None,
  Stmnts(List(DefExpr(("a"), None, Stmnts(
    List(IfExpr(BoolLit(true), Stmnts(List(LVar("b"))), Nil, Some((Stmnts(List(LVar("c"))))))))
  )))))
        }
      }
    }
  }

  def pp(ast: AST): String = PrettyPrinter.call(ast)
}
