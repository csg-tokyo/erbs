package rbparser

import org.scalatest._

object Debug extends Tag("Debug")

class ParserTest extends FunSpec {
  describe ("Literal") {
    it ("returns Int Literal") {
      parse("1") { v => assert(v == IntLit(1)) }
      parse("100") { v => assert(v == IntLit(100)) }
    }

    it ("returns Double Literal") {
      parse("3.14") { v => assert(v == DoubleLit(3.14)) }
      parse("0.34") { v => assert(v == DoubleLit(0.34)) }
    }

    it ("returns Id Literal") {
      parse("x") { v => assert(v == LVar("x")) }
      parse("x2") { v => assert(v == LVar("x2")) }
      parse("ends") { v => assert(v == LVar("ends")) }
      parse("_end") { v => assert(v == LVar("_end")) }
      parse("end_") { v => assert(v == LVar("end_")) }
    }

    it ("returns String Literal") {
      parse(""""asdf"""") { v =>  assert(v == StringLit("\"asdf\"")) }
      parse(""""asdf\n"""") { v =>  assert(v == StringLit("\"asdf\\n\"")) }
      parse(""""as\ndf\n"""") { v =>  assert(v == StringLit("\"as\\ndf\\n\"")) }
      parse(""""as\"df"""") { v =>  assert(v == StringLit("\"as\\\"df\"")) }
    }

    it ("returns BoolLit wrapped value") {
      parse("true") { v => assert(v == BoolLit(true)) }
    }

    it ("returns Const wrapped value") {
      parse("ASDF") { v =>  assert(v == ConstLit("ASDF")) }
      parse("ASDF2") { v => assert(v == ConstLit("ASDF2")) }
    }

    it ("returns symbol wrapped value") {
      parse(":a") { v =>  assert(v == SymbolLit("a")) }
      parse(":a1") { v =>  assert(v == SymbolLit("a1")) }
      parse(":_a1") { v =>  assert(v == SymbolLit("_a1")) }
      parse(":__a1") { v =>  assert(v == SymbolLit("__a1")) }
      parse(":A") { v =>  assert(v == SymbolLit("A")) }
    }

    it ("returns instance variable wrapped value") {
      parse("@a") { v =>  assert(v == IVar("a")) }
      parse("@a1") { v =>  assert(v == IVar("a1")) }
      parse("@_a") { v => assert(v == IVar("_a")) }
      parse("@_a1") { v => assert(v == IVar("_a1")) }
      parse("@__a1") { v => assert(v == IVar("__a1")) }
      parse("@A") { v => assert(v == IVar("A")) }
    }
  }

  // describe("stmnt") {
  //   parse("return") { v =>  assert(v == Unary(EXT(), IVar("a"))) }
  // }

  describe("Ex") {
    parse("!@a") { v =>  assert(v == Unary(EXT(), IVar("a"))) }
    parse("!true") { v =>  assert(v == Unary(EXT(), BoolLit(true))) }
    parse("!a") { v =>  assert(v == Unary(EXT(), LVar("a"))) }
    // parse("!(true && false)") { v =>  assert(v == Unary(EXT(), LVar("a"))) }
    parse("!A") { v =>  assert(v == Unary(EXT(), ConstLit("A"))) }
  }

  describe("minus") {
    parse("-a") { v =>  assert(v == Unary(MINUS(), LVar("a"))) }
    parse("-A") { v =>  assert(v == Unary(MINUS(), ConstLit("A"))) }
    parse("-10") { v =>  assert(v == Unary(MINUS(), IntLit(10))) }
    parse("-10.0") { v =>  assert(v == Unary(MINUS(), DoubleLit(10.0))) }
    parse("-(1+2)") { v =>  assert(v == Unary(MINUS(), Prim(PLUS(), IntLit(1), IntLit(2)))) }
  }

  describe("Prim") {
    parse("1 + 2") { v => assert(v == Prim(PLUS(), IntLit(1), IntLit(2))) }
    parse("1 + -2") { v => assert(v == Prim(PLUS(), IntLit(1), Unary(MINUS(), IntLit(2)))) }
    parse("-2 + 1") { v => assert(v == Prim(PLUS(), Unary(MINUS(), IntLit(2)), IntLit(1))) }
    parse("1 + (2 + 2)") { v => assert(v == Prim(PLUS(), IntLit(1), Prim(PLUS(), IntLit(2), IntLit(2)))) }
    parse("1 - 2") { v => assert(v == Prim(MINUS(), IntLit(1), IntLit(2))) }
    parse("1 * 2") { v => assert(v == Prim(AST(), IntLit(1), IntLit(2))) }
    parse("1 / 2") { v => assert(v == Prim(DIV(), IntLit(1), IntLit(2))) }
    parse("i / 2") { v => assert(v == Prim(DIV(), LVar("i"), IntLit(2))) }
    parse("@a / 2") { v => assert(v == Prim(DIV(), IVar("a"), IntLit(2))) }
    parse("1 - 2 * 3") { v => assert(v == Prim(MINUS(), IntLit(1), Prim(AST(), IntLit(2), IntLit(3)))) }
    // parse("1 - 2 - 3") { v => assert(v == Prim(MINUS(), Prim(MINUS(), IntLit(1), IntLit(2)), IntLit(3))) }
    // parse("1 + 2 * 1 + 10") { v => assert(
    //   v == Prim(PLUS(), Prim(PLUS(), IntLit(1), Prim(AST(), IntLit(2), IntLit(1))), IntLit(10))) }

    describe("Cmp") {
      parse("1 < 2") { v => assert(v == Prim(LT(), IntLit(1), IntLit(2))) }
      parse("1 >= 2") { v => assert(v == Prim(GE(), IntLit(1), IntLit(2))) }
      // parse("1 + 2 >= 3") { v => assert(v == Prim(GE(), Prim(PLUS(), IntLit(1), IntLit(2)), IntLit(3))) }
    }

    describe ("Condtion") {
      parse("true && false") { v => assert(v == Prim(AND(), BoolLit(true), BoolLit(false))) }
      // parse("true || false && false") { v => assert(v == Prim(OR(), BoolLit(true), Prim(AND(), BoolLit(false), BoolLit(false)))) }
    }
  }

  describe("Assign") {
    parse("a = 1 + 2") { v => assert(v == Assign(LVar("a"), Prim(PLUS(), IntLit(1), IntLit(2)))) }
    parse("@a = 1 + 2") { v => assert(v == Assign(IVar("a"), Prim(PLUS(), IntLit(1), IntLit(2)))) }
    parse("a = a = 1 + 2") { v => assert(v == Assign(LVar("a"), Assign(LVar("a"), Prim(PLUS(), IntLit(1), IntLit(2))))) }

    // describe("and OR") {
    // parse("@a ||= 1 + 2") { v => assert(v == Assign(ORE(), IVar("a"), Prim(PLUS(), IntLit(1), IntLit(2)))) }
    // }
  }

  describe("Method call") {
    parse("@a.call") { v => assert(v == Call(Some(IVar("a")), MethodName("call"), None)) }
    parse("b.call(a)") { v => assert(v == Call(Some(LVar("b")), MethodName("call"), Some(ActualArgs(List(LVar("a")))))) }
    parse("call 10") { v => assert(v == Call(None, MethodName("call"), Some(ActualArgs(List(IntLit(10)))))) }
    parse("attr_reader :a, :b") { v => assert(v == Call(None, MethodName("attr_reader"), Some(ActualArgs(List(SymbolLit("a"), SymbolLit("b")))))) }
    parse("a = @a.call") { v => assert(v == Assign(LVar("a"), Call(Some(IVar("a")), MethodName("call"), None))) }
    parse("A.new") { v => assert(v == Call(Some(ConstLit("A")), MethodName("new"), None)) }
    parse("a1._call(10)") { v => assert(v == Call(Some(LVar("a1")), MethodName("_call"), Some(ActualArgs(List(IntLit(10)))))) }
    parse("a1._calL?(10)") { v => assert(v == Call(Some(LVar("a1")), MethodName("_calL?"), Some(ActualArgs(List(IntLit(10)))))) }
    parse("a1._calL!(10)") { v => assert(v == Call(Some(LVar("a1")), MethodName("_calL!"), Some(ActualArgs(List(IntLit(10)))))) }

    // parse("a.call() < 10") { v => assert(v == Prim(LT(), Call(Some(LVar("a")), MethodName("call"), None), IntLit(10))) }
    // parse("a1.call()") { v => assert(v == Call(Some(LVar("a1")), MethodName("call"), None)) }
    // parse("@a.call + 10") { v => assert(v == Call(Some(IVar("a")), MethodName("call"), None)) }
    // parse("a1.call(10, true)") { v => assert(v == Call(Some(LVar("a1")), MethodName("call"), Some(ActualArgs(List(IntLit(10)))))) }
    // parse("!a1._calL?(10)") { v => assert(v == Unary(EXT(),Call(Some(LVar("a1")), MethodName("_calL?"), Some(ActualArgs(List(IntLit(10))))))) }
  }

  describe("if") {
    parse("""if  true
   1 + 2
  end""") { v => assert(v == IfExpr(BoolLit(true), Stmnts(List(Prim(PLUS(),IntLit(1),IntLit(2)))))) }

      parse("""if a(10)
     b
    end""") { v => assert(v == IfExpr(Call(None, MethodName("a"), Some(ActualArgs(List(IntLit(10))))), Stmnts(List(LVar("b"))))) }
  }

  describe("Class") {
    it ("retruns class", Tag("asdf")) {
      parse("""class A
  end""") { v => assert(v == ClassExpr(ConstLit("A"), Stmnts(List()))) }

      parse("""class A
    def a
     1 + 2
    end
  end
  """) { v => assert(v == ClassExpr(ConstLit("A"), Stmnts(List(
    DefExpr(MethodName("a"), None, Stmnts(List(Prim(PLUS(), IntLit(1), IntLit(2)))))))))
      }
    }
  }

  describe("def") {
    it ("retruns class") {
      parse("""def a
  end""") { v => assert(v == DefExpr(MethodName("a"), None, Stmnts(List())))}
    }
    parse("""def a?
  end""") { v => assert(v == DefExpr(MethodName("a?"), None, Stmnts(List())))}

    parse("""def ASDF?
  end""") { v => assert(v == DefExpr(MethodName("ASDF?"), None, Stmnts(List())))}

    parse("""def _a?
  end""") { v => assert(v == DefExpr(MethodName("_a?"), None, Stmnts(List())))}

    parse("""def a?()
  end""") { v => assert(v == DefExpr(MethodName("a?"), None, Stmnts(List())))}

    parse("""def a()
  end""") { v => assert(v == DefExpr(MethodName("a"), None, Stmnts(List())))}

    parse("""def a(opt)
  end""") { v => assert(v == DefExpr(MethodName("a"), Some(FormalArgs(List(LVar("opt")))), Stmnts(List())))}

    parse("""def call(a, b)
  end""") { v => assert(v == DefExpr(MethodName("call"), Some(FormalArgs(List(LVar("a"), LVar("b")))), Stmnts(List())))}

    parse("""def call()
   1 + 2
  end""") { v => assert(v == DefExpr(MethodName("call"), None, Stmnts(List(Prim(PLUS(), IntLit(1), IntLit(2)))))) }
  }

  def parse(x: String)(fn: Expr => Unit): Unit = {
    val parser = new Parser()
    parser.parse(x + "\n") match {
      case Right(Stmnts(x)) => fn(x(0))
      case Right(x) => throw new Exception(s"invalid type: $x")
      case Left(s) => throw new Exception(s)
    }
  }
}
