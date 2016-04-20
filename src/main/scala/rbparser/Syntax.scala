package rbparser

sealed trait Op { val prec: Int = 0 }
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
case class DOT() extends Op    { override val prec: Int = 30 }

case class MethodName(v: String)
case class FormalArgs(name: List[LVar])
case class ActualArgs(name: List[Expr])

sealed trait Syntax
sealed trait Literal extends Expr
case class IntLit(v: Int) extends Literal
case class DoubleLit(v: Double) extends Literal
case class BoolLit(v: Boolean) extends Literal
case class ConstLit(v: String) extends Literal
case class SymbolLit(v: String) extends Literal
case class StringLit(v: String) extends Literal
case class LVar(v: String) extends Literal
case class IVar(v: String) extends Literal

sealed trait Expr extends Syntax
case class ARef(v: Expr, ref: Expr) extends Expr
case class Ary(v: List[Expr]) extends Expr
case class IfExpr(cond: Expr, t_body: Stmnts) extends Expr
case class Return(args: List[Expr]) extends Expr
case class UnlessExpr(cond: Expr, t_body: Stmnts) extends Expr
case class Unary(op: Op, v: Expr) extends Expr
case class Binary(v: Op, lht: Expr, rht: Expr) extends Expr
case class Call(rev: Option[Expr], name: MethodName, args: Option[ActualArgs]) extends Expr
case class Assign(id: Expr, value: Expr) extends Expr
case class ClassExpr(name: ConstLit, body: Stmnts) extends Expr
case class DefExpr(name: MethodName, args: Option[FormalArgs], body: Stmnts) extends Expr

case class Stmnts(v: List[Expr]) extends Syntax
