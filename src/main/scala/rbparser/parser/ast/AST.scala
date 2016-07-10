package rbparser.parser.ast

import rbparser.parser.ast.concerns.MethodTranslate

sealed trait Op { val prec: Int = 0 }
case class PLUS() extends Op  { override val prec: Int = 10 }
case class MINUS() extends Op { override val prec: Int = 10 }
case class MUL() extends Op   { override val prec: Int = 11 }
case class DIV() extends Op   { override val prec: Int = 11 }
case class EXT() extends Op   { override val prec: Int = 20 } // !
case class GT() extends Op    { override val prec: Int = 8 } // >
case class GE() extends Op    { override val prec: Int = 8 } // >=
case class LT() extends Op    { override val prec: Int = 8 } // <
case class LE() extends Op    { override val prec: Int = 8 } // <=
case class AND() extends Op   { override val prec: Int = 6 } // &&
case class ANDE() extends Op   { override val prec: Int = 6 } // &&=
case class OR() extends Op    { override val prec: Int = 5 } // ||
case class ORE() extends Op    { override val prec: Int = 3 } // ||= TODO add assocative
case class ADDE() extends Op    { override val prec: Int = 6 } // +=
case class SUBE() extends Op    { override val prec: Int = 6 } // -=
case class EQ() extends Op    { override val prec: Int = 6 } // =
case class DOT() extends Op    { override val prec: Int = 30 }

object Op {
  def stringfy(op: Op) = op match {
    case PLUS() => "+"
    case MINUS() => "-"
    case MUL() => "*"
    case DIV() => "/"
    case EXT() => "!"
    case GT() => ">"
    case GE() => ">="
    case LT() => "<"
    case LE() => "<="
    case AND() => "&&"
    case OR() => "||"
    case ORE() => "||="
    case DOT() => "."
    case ANDE() => "&&="
    case ADDE() => "+="
    case SUBE() => "-="
    case EQ() => "="
  }
}

case class FormalArgs(names: List[LVar]) extends AST
case class ActualArgs(names: List[Expr]) extends AST

sealed trait AST
sealed trait Literal extends Expr
case class IntLit(v: Int) extends Literal
case class DoubleLit(v: Double) extends Literal
case class BoolLit(v: Boolean) extends Literal
case class ConstLit(v: String) extends Literal
case class SymbolLit(v: String) extends Literal
case class StringLit(v: String) extends Literal
case class LVar(v: String) extends Literal
case class IVar(v: String) extends Literal
case class Keyword(v: String) extends Literal

sealed trait Expr extends AST
case class ARef(v: Expr, ref: Expr) extends Expr
case class Ary(v: List[Expr]) extends Expr
case class Hash(v: Map[Expr, Expr]) extends Expr
case class IfExpr(cond: Expr, t_body: Stmnts) extends Expr
case class IfModExpr(cond: Expr, expr: Expr) extends Expr
case class UnlessExpr(cond: Expr, t_body: Stmnts) extends Expr
case class UnlessModExpr(cond: Expr, t_body: Expr) extends Expr
case class Return(args: List[Expr]) extends Expr
case class Unary(op: Op, v: Expr) extends Expr
case class Binary(v: Op, lht: Expr, rht: Expr) extends Expr
case class Call(rev: Option[Expr], name: String, args: Option[ActualArgs], block: Option[Block]) extends Expr
case class Cmd(rev: Option[Expr], name: String, args: Option[ActualArgs], block: Option[Block]) extends Expr
case class Assign(target: Expr, value: Expr, op: Op) extends Expr
case class ClassExpr(name: ConstLit, body: Stmnts) extends Expr
case class ModuleExpr(name: ConstLit, body: Stmnts) extends Expr
case class DefExpr(name: String, args: Option[FormalArgs], body: Stmnts) extends Expr

sealed abstract class Block(args: Option[ActualArgs], body: Stmnts) extends Expr
case class DoBlock(args: Option[ActualArgs], body: Stmnts) extends Block(args, body)
case class BraceBlock(args: Option[ActualArgs], body: Stmnts) extends Block(args, body)

case class Stmnts(v: List[Expr]) extends AST {
  def map(f: Expr => Expr): Stmnts = Stmnts(v.map(f))
}

// extendted
case class Operators(ops: List[Operator]) extends Expr
case class Operator(tags: Set[String], syntax: Syntax, body: Expr) extends Expr with MethodTranslate {
  val syntaxBody: List[String] = syntax.body
  val syntaxTags: Map[String, Expr] = syntax.tags
}
case class Syntax(tags: Map[String, Expr], body: List[String]) extends Expr
