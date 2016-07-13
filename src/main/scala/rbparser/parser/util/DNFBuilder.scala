// Builder of Disjunctive Normal Form
package rbparser.parser.util

import rbparser.parser.ast._

object DNFBuilder {
  def build(e :Expr): Expr = e match {
    case Binary(AND, l, r) => (build(l), build(r)) match {
      case (Binary(OR, e1, e2), Binary(OR, e3, e4)) => {
        Binary(OR,
          Binary(OR, build(Binary(AND, e1, e3)), build(Binary(AND, e1, e4))),
          Binary(OR, build(Binary(AND, e2, e3)), build(Binary(AND, e2, e4))))
      }
      case (e1, Binary(OR, e2, e3)) => Binary(OR, build(Binary(AND, e1, e2)), build(Binary(AND, e1, e3)))
      case (Binary(OR, e1, e2), e3) => Binary(OR, build(Binary(AND, e1, e3)), build(Binary(AND, e2, e3)))
      case (x, y) => Binary(AND, x, y)
    }
    case Unary(EXT, e) => e match {
      case Binary(OR, l, r) => build(Binary(AND, build(Unary(EXT, l)), build(Unary(EXT, r))))
      case Binary(AND, l, r) => Binary(OR, build(Unary(EXT, l)), build(Unary(EXT, r)))
      case Unary(EXT, e2) => build(e2)
      case e@LVar(_) => Unary(EXT, e)
      case x => x
    }
    case Binary(OR, e1, e2) => Binary(OR, build(e1), build(e2))
    case x => x
  }
}
