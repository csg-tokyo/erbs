package rbparser

import org.scalatest._

class MethodTranslateTest extends FunSpec {
  describe ("toMethodCall") {
    it ("convert methodcall with simple function") {
      val syntax = Syntax(Map("b" -> LVar("origin")), List("a", "b"))
      val op = Operator(Set("origin"), syntax, IntLit(10))

      assertResult(Call(None, "Operator::Origin::op_a_B", Some(ActualArgs(List(IntLit(10)))),None)) {
        op.toMethodCall(Map("b" -> IntLit(10)))
      }
    }

    it ("convert methodcall with name including symbol") {
      val syntax = Syntax(Map("b" -> LVar("origin")), List("<=", "b"))
      val op = Operator(Set("origin"), syntax, IntLit(10))

      assertResult(Call(None, "Operator::Origin::op_6061_B", Some(ActualArgs(List(IntLit(10)))),None)) {
        op.toMethodCall(Map("b" -> IntLit(10)))
      }
    }
  }

  describe ("toMethod") {
    it ("converts operator to method") {
      val syntax = Syntax(Map("b" -> LVar("origin")), List("<=", "b"))
      val op = Operator(Set("origin"), syntax, IntLit(10))

      assertResult(DefExpr("self.op_6061_B", Some(FormalArgs(List(LVar("b")))), Stmnts(List(IntLit(10))))) {
        op.toMethod
      }
    }
  }
}
