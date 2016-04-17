package rbparser

sealed trait Var // TODO fix
case class MethodName(v: String)

sealed trait Syntax
sealed trait Expr extends Syntax
sealed trait Literal extends Expr

sealed trait Op {
  val prec: Int = 0
}

case class PLUS() extends Op  { override val prec: Int = 10 }
case class MINUS() extends Op { override val prec: Int = 10 }
case class AST() extends Op   { override val prec: Int = 11 }
case class DIV() extends Op   { override val prec: Int = 11 }
case class EXT() extends Op   { override val prec: Int = 20 } // !
case class GT() extends Op    { override val prec: Int = 8 } // >
case class GE() extends Op    { override val prec: Int = 8 } // >=
case class LT() extends Op    { override val prec: Int = 8 } // <
case class LE() extends Op    { override val prec: Int = 8 } // <=
case class AND() extends Op   { override val prec: Int = 6 } // &&
case class OR() extends Op    { override val prec: Int = 5 } // ||
case class ORE() extends Op    { override val prec: Int = 3 } // ||= TODO add assocative

case class IntLit(v: Int) extends Literal
case class DoubleLit(v: Double) extends Literal
case class BoolLit(v: Boolean) extends Literal
case class IdLit(v: String) extends Literal with Var
case class InstVarLit(v: String) extends Literal with Var
case class ConstLit(v: String) extends Literal

case class Unary(op: Op, v: Expr) extends Expr
case class Prim(v: Op, lht: Expr, rht: Expr) extends Expr
case class ClassExpr(name: ConstLit, body: Stmnts) extends Expr
case class DefExpr(name: MethodName, args: Option[FormalArgs], body: Stmnts) extends Expr
case class FormalArgs(name: List[IdLit]) extends Expr
case class ActualArgs(name: List[Expr]) extends Expr
case class Assign(id: Var, value: Expr) extends Expr
case class MCall(rev: Expr, name: MethodName, args: Option[ActualArgs]) extends Expr

case class Stmnts(v: List[Expr]) extends Syntax
