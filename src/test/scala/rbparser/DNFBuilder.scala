package rbparser

import org.scalatest._

class DNFBuilderTest extends FunSpec {

  describe ("Primitive Form") {
    it ("returns same as a input") {
      assertResult(LVar("a")) {
        DNFBuilder.build(LVar("a"))
      }
      assertResult(Binary(OR(), LVar("a"), LVar("b"))) {
	      DNFBuilder.build(Binary(OR(), LVar("a"), LVar("b")))
      }
      assertResult(Binary(OR(), Binary(AND(), LVar("a"), LVar("b")), LVar("c")), "(a && b) || c") {
	      DNFBuilder.build(Binary(OR(), Binary(AND(), LVar("a"), LVar("b")), LVar("c")))
      }
    }
  }

  describe ("And From") {
    it ("retutrn DNF") {
      assertResult(Binary(OR(), Binary(AND(), LVar("a"), LVar("c")), Binary(AND(), LVar("b"), LVar("c"))), "(a || b) && c") {
        DNFBuilder.build(Binary(AND(), Binary(OR(), LVar("a"), LVar("b")), LVar("c")))
      }
      assertResult(Binary(OR(), Binary(AND(), LVar("a"), LVar("b")), Binary(AND(), LVar("a"), LVar("c"))), "a && (b || c)") {
        DNFBuilder.build(Binary(AND(), LVar("a"), Binary(OR(), LVar("b"), LVar("c"))))
      }
      assertResult(Binary(OR(),
        Binary(OR(), Binary(AND(), LVar("a"), LVar("c")), Binary(AND(), LVar("a"), LVar("d"))),
        Binary(OR(), Binary(AND(), LVar("b"), LVar("c")), Binary(AND(), LVar("b"), LVar("d")))
      ), "(a || b) && (c || d) => (a && c) || (a && d) || (b && c) || (b && d)") {
        DNFBuilder.build(Binary(AND(), Binary(OR(), LVar("a"), LVar("b")), Binary(OR(), LVar("c"), LVar("d"))))
      }
      assertResult(Binary(OR(),
        Binary(AND(), Binary(AND(), LVar("a"), LVar("c")), LVar("d")),
        Binary(AND(), Binary(AND(), LVar("b"), LVar("c")), LVar("d")))
        , "((a || b) && c) && d => (a && c && d) || (b && c && d)") {
        DNFBuilder.build(Binary(AND(), Binary(AND(), Binary(OR(), LVar("a"), LVar("b")), LVar("c")), LVar("d")))
      }
    }
  }

  describe("Not Predicate") {
    it ("returns DNF") {
      assertResult(LVar("a"), "!(!a)") {
        DNFBuilder.build(Unary(EXT(), Unary(EXT(), LVar("a"))))
      }
      assertResult(Binary(AND(), Unary(EXT(), LVar("a")), Unary(EXT(), LVar("b"))), "!(b || c)") {
        DNFBuilder.build(Unary(EXT(), Binary(OR(), LVar("a"), LVar("b"))))
      }
      assertResult(Binary(OR(), Unary(EXT(), LVar("a")), Unary(EXT(), LVar("b"))), "!(b && c)") {
        DNFBuilder.build(Unary(EXT(), Binary(AND(), LVar("a"), LVar("b"))))
      }
      assertResult(Binary(OR(), Unary(EXT(), LVar("a")), Binary(OR(), Unary(EXT(), LVar("b")), Unary(EXT(), LVar("c")))), "!(a && (b && c))") {
        DNFBuilder.build(Unary(EXT(), Binary(AND(), LVar("a"), Binary(AND(), LVar("b"), LVar("c")))))
      }
      assertResult(Binary(OR(), Unary(EXT(), LVar("a")), Binary(AND(), Unary(EXT(), LVar("b")), Unary(EXT(), LVar("c")))), "!(a && (b || c))") {
        DNFBuilder.build(Unary(EXT(), Binary(AND(), LVar("a"), Binary(OR(), LVar("b"), LVar("c")))))
      }
      assertResult(Binary(OR(), Binary(AND(), Unary(EXT(),LVar("a")), Unary(EXT(), LVar("b"))), Binary(AND(), Unary(EXT(), LVar("a")), Unary(EXT(),LVar("c")))), "!(a || (b && c))") {
        DNFBuilder.build(Unary(EXT(), Binary(OR(), LVar("a"), Binary(AND(), LVar("b"), LVar("c")))))
      }
      assertResult(Binary(OR(), Binary(OR(), Binary(AND(), Unary(EXT(), LVar("a")), Binary(AND(), Unary(EXT(),LVar("b")), Unary(EXT(),LVar("d")))), Binary(AND(), Unary(EXT(), LVar("a")), Binary(AND(), Unary(EXT(), LVar("b")), Unary(EXT(), LVar("e"))))),Binary(OR(), Binary(AND(), Unary(EXT(), LVar("a")), Binary(AND(), Unary(EXT(), LVar("c")), Unary(EXT(), LVar("d")))), Binary(AND(), Unary(EXT(), LVar("a")), Binary(AND(), Unary(EXT(), LVar("c")), Unary(EXT(), LVar("e")))))), "!(a || (b && c) || (d && e))") {
        DNFBuilder.build(Unary(EXT(), Binary(OR(), LVar("a"), Binary(OR(), Binary(AND(), LVar("b"), LVar("c")), Binary(AND(), LVar("d"), LVar("e"))))))
      }
    }
  }
}
