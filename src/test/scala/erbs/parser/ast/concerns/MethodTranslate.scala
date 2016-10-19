package erbs.parser.ast
package concerns

import org.scalatest._

class MethodTranslateTest extends FunSpec {
  describe ("#toMethodCall") {
    it ("converts operator to an AST of the function call") {
      val syntax = Syntax(Map("b" -> LVar("origin")), List("a", "b"))
      val op = Operator(Set("origin"), syntax, Stmnts(List(IntLit(10))))

      assertResult(Call(None, "Operator::Origin::op_a_B", Some(ActualArgs(List(IntLit(10)))),None)) {
        op.toMethodCall(Map("b" -> IntLit(10)))
      }
    }

    it ("converts operator to an AST of the function even if a function name has a symbol") {
      val syntax = Syntax(Map("b" -> LVar("origin")), List("<=", "b"))
      val op = Operator(Set("origin"), syntax, Stmnts(List(IntLit(10))))

      assertResult(Call(None, "Operator::Origin::op_6061_B", Some(ActualArgs(List(IntLit(10)))),None)) {
        op.toMethodCall(Map("b" -> IntLit(10)))
      }
    }
  }

  describe ("#toMethodDefinition") {
    it ("converts operator to method") {
      val syntax = Syntax(Map("b" -> LVar("origin")), List("<=", "b"))
      val op = Operator(Set("origin"), syntax, Stmnts(List(IntLit(10))))

      assertResult(DefExpr("self.op_6061_B", Some(FormalArgs(List(LVar("b")))), Stmnts(List(IntLit(10))))) {
        op.toMethodDefinition
      }
    }
  }
}
