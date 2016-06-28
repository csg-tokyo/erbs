package rbparser

object OperatorBuilder {
  val name = "Operator"

  def buildDefinintion(m: Map[String, List[DefExpr]]) = {
    val body = m.toList.map { case (k, v) => ClassExpr(ConstLit(k), Stmnts(v)); }
    ModuleExpr(ConstLit(name), Stmnts(body))
  }

  def build(op: Operator): DefExpr = {
    val name = "self." + methodName(op)
    val args = op.syntax.tags.keys.toList.map(LVar(_)) match {
      case Nil => None
      case x => Some(FormalArgs(x))
    }
    DefExpr(name, args, Stmnts(List(op.body)))
  }

  def baseName(op: Operator) = s"${name}::${op.className}"

  def methodName(op: Operator) = op.syntax.body.map { x => op.syntax.tags.get(x).fold(x)(_ => "X") }.mkString("_")

  def callingName(op: Operator) = s"${baseName(op)}::${methodName(op)}"
}
