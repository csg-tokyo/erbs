package erbs.parser

import org.scalatest._
import erbs.parser.ast.{ATToken, Unary, EXT, Binary, AND}

class ContextTest extends FunSpec {
  describe ("isEmpty") {
    it ("return true when context is empty") {
      assertResult(true) { Context().isEmpty }
    }
    it ("false when ok has a value") {
      assertResult(false) { Context(Set("a")).isEmpty }
    }
    it ("false when ng has a value") {
      assertResult(false) { Context(Set(), Set("a")).isEmpty }
    }
    it ("false when ng and ok have values") {
      assertResult(false) { Context(Set("a"), Set("b")).isEmpty }
    }
  }

  describe ("contextSeq") {
    it ("return a empty sequence when context is empty") {
      assertResult(Seq()) { Context().contextSeq }
    }
    it ("return a sequence of ATToken when ok has a value") {
      assertResult(Seq(ATToken("a"))) { Context(Set("a")).contextSeq }
    }
    it ("return a sequence of !ATToken when ng has a value") {
      assertResult(Seq(Unary(EXT, ATToken("a")))) { Context(Set(), Set("a")).contextSeq }
    }
    it ("return a sequence of ATToken and !ATToken when ok and ng have values") {
      assertResult(Seq(ATToken("b"), ATToken("c"), Unary(EXT, ATToken("a")), Unary(EXT, ATToken("d")))) {
        Context(Set("b", "c"), Set("a", "d")).contextSeq
      }
    }
  }

  describe ("contextExpr") {
    it ("return None when context is empty") {
      assertResult(None) { Context().contextExpr }
    }
    it ("return ATToken wrapped by Option when ok has a value") {
      assertResult(Some(ATToken("a"))) { Context(Set("a")).contextExpr }
    }
    it ("return !ATToken wrapped by Option when ng has a value") {
      assertResult(Some(Unary(EXT, ATToken("a")))) { Context(Set(), Set("a")).contextExpr }
    }
    it ("return Binary wrapped by Option when context has multiple value") {
      assertResult(Some(Binary(AND, ATToken("a"), Unary(EXT, ATToken("b"))))) {
        Context(Set("a"), Set("b")).contextExpr
      }
    }
  }

  describe ("fold") {
    it ("return a passed default value when context is empty") {
      val v = "default value"
      assertResult(v) { Context().fold(v)(e => "") }
    }
    it ("invoke a passed function with a contextExpr when context has values") {
      assertResult("ab") {
        Context(Set("a"), Set("b")).fold("") {
          case Binary(AND, ATToken(c), Unary(EXT, ATToken(nc))) => c + nc
          case _ => "nope"
        }
      }
    }
  }

  describe ("createNewContext") {
    val c = Context(Set("a"), Set("c"))

    it ("return new context")  {
      assertResult(Context(Set("a"), Set("c"))) {
        c.cloneWith((Set(), Set()))
      }
    }

    it ("create new context and return it")  {
      assertResult(Context(Set("a", "b"), Set("c", "d"))) {
        c.cloneWith((Set("b"), Set("d")))
      }
    }
  }
}
