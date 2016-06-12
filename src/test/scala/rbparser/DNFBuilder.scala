package rbparser

import org.scalatest._
// import Matchers._

class DNFBuilderTest extends FunSpec {
  def assetWithBuild(expect: Expr, clue: String = "")(actualResult: => Expr) = assertResult(expect, clue) { DNFBuilder.build(actualResult) }

  describe ("Primitive Form") {
    it ("returns same as a input") {
      assetWithBuild(LVar("a")) { LVar("a") }
      assetWithBuild(Binary(OR(), LVar("a"), LVar("b"))) { Binary(OR(), LVar("a"), LVar("b")) }
      assetWithBuild(Binary(OR(), Binary(AND(), LVar("a"), LVar("b")), LVar("c")), "(a && b) || c") {
        Binary(OR(), Binary(AND(), LVar("a"), LVar("b")), LVar("c"))
      }
    }
  }

  describe ("And From") {
    it ("retutrn DNF") {
      assetWithBuild(Binary(OR(), Binary(AND(), LVar("a"), LVar("c")), Binary(AND(), LVar("b"), LVar("c"))), "(a || b) && c") {
        Binary(AND(), Binary(OR(), LVar("a"), LVar("b")), LVar("c"))
      }
      assetWithBuild(Binary(OR(), Binary(AND(), LVar("a"), LVar("b")), Binary(AND(), LVar("a"), LVar("c"))), "a && (b || c)") {
        Binary(AND(), LVar("a"), Binary(OR(), LVar("b"), LVar("c")))
      }
      assetWithBuild(Binary(OR(),
        Binary(OR(), Binary(AND(), LVar("a"), LVar("c")), Binary(AND(), LVar("a"), LVar("d"))),
        Binary(OR(), Binary(AND(), LVar("b"), LVar("c")), Binary(AND(), LVar("b"), LVar("d")))
      ), "(a || b) && (c || d) => (a && c) || (a && d) || (b && c) || (b && d)") {
        Binary(AND(), Binary(OR(), LVar("a"), LVar("b")), Binary(OR(), LVar("c"), LVar("d")))
      }
      assetWithBuild(Binary(OR(),
        Binary(AND(), Binary(AND(), LVar("a"), LVar("c")), LVar("d")),
        Binary(AND(), Binary(AND(), LVar("b"), LVar("c")), LVar("d")))
        , "((a || b) && c) && d => (a && c && d) || (b && c && d)") {
        Binary(AND(), Binary(AND(), Binary(OR(), LVar("a"), LVar("b")), LVar("c")), LVar("d"))
      }
    }
  }
}
