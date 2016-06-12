// Builder of Disjunctive Normal Form
package rbparser

object DNFBuilder {
  def build(e :Expr): Expr = e match {
    case Binary(AND(), l, r) => (build(l), build(r)) match {
      case (Binary(OR(), e1, e2), Binary(OR(), e3, e4)) => {
        Binary(OR(),
          Binary(OR(), Binary(AND(), e1, e3), Binary(AND(), e1, e4)),
          Binary(OR(), Binary(AND(), e2, e3), Binary(AND(), e2, e4)))
      }
      case (e1, Binary(OR(), e2, e3)) => Binary(OR(), Binary(AND(), e1, e2), Binary(AND(), e1, e3))
      case (Binary(OR(), e1, e2), e3) => Binary(OR(), Binary(AND(), e1, e3), Binary(AND(), e2, e3))
      case (x, y) => Binary(AND(), x, y)
    }
    case Binary(OR(), e1, e2) => Binary(OR(), build(e1), build(e2))
    case x => x
  }
}
