package rbparser

import org.scalatest.FunSpec
import rbparser._

class ParserTest extends FunSpec {
  val parser = new Parser

  describe ("Literal") {

    it ("returns Int Literal") {
      assert(parseLine("1") == IntLit(1))
      assert(parseLine("100") == IntLit(100))
    }

    it ("returns Double Literal") {
      assert(parseLine("3.14") == DoubleLit(3.14))
      assert(parseLine("0.34") == DoubleLit(0.34))
    }

    it ("returns Id Literal") {
      assert(parseLine("x") == IdLit("x"))
      assert(parseLine("xS2") == IdLit("xS2"))
    }

    it ("returns BoolLit wrapped value") {
      assert(parseLine("true") == BoolLit(true))
    }
  }
}
