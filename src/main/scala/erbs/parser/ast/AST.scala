package erbs.parser.ast

import concerns.MethodTranslate

sealed trait Op { val prec: Int = 0 }
case object PLUS extends Op  { override val prec: Int = 10 }
case object MINUS extends Op { override val prec: Int = 10 }
case object MUL extends Op   { override val prec: Int = 11 }
case object DIV extends Op   { override val prec: Int = 11 }
case object EXT extends Op   { override val prec: Int = 20 } // !
case object GT extends Op    { override val prec: Int = 8 } // >
case object GE extends Op    { override val prec: Int = 8 } // >=
case object LT extends Op    { override val prec: Int = 8 } // <
case object LE extends Op    { override val prec: Int = 8 } // <=
case object AND extends Op   { override val prec: Int = 6 } // &&
case object ANDE extends Op   { override val prec: Int = 6 } // &&=
case object OR extends Op    { override val prec: Int = 5 } // ||
case object ORE extends Op    { override val prec: Int = 3 } // ||= TODO add assocative
case object ADDE extends Op    { override val prec: Int = 6 } // +=
case object SUBE extends Op    { override val prec: Int = 6 } // -=
case object EQ extends Op    { override val prec: Int = 6 } // =
case object EEQ extends Op    { override val prec: Int = 6 } // ==
case object DOT extends Op    { override val prec: Int = 30 }

object Op {
  def stringfy(op: Op) = op match {
    case PLUS => "+"
    case MINUS => "-"
    case MUL => "*"
    case DIV => "/"
    case EXT => "!"
    case GT => ">"
    case GE => ">="
    case LT => "<"
    case LE => "<="
    case AND => "&&"
    case OR => "||"
    case ORE => "||="
    case DOT => "."
    case ANDE => "&&="
    case ADDE => "+="
    case SUBE => "-="
    case EEQ => "=="
    case EQ => "="
  }
}


sealed trait ArgElement extends AST {
  def value: Expr
}
case class SimpleArgElement(val value: LVar) extends ArgElement
case class KeywordArgElement(val key: SymbolLit, val value: Expr) extends ArgElement
case class DefaultArgElement(val key: LVar, val value: Expr) extends ArgElement
case class ActualArgElement(val value: Expr) extends ArgElement

case class FormalArgs(val args: List[ArgElement]) extends AST
case class ActualArgs(val args: List[ArgElement]) extends AST

sealed trait AST
sealed trait Literal extends Expr
case class IntLit(v: Int) extends Literal
case class DoubleLit(v: Double) extends Literal
case class BoolLit(v: Boolean) extends Literal
case class ConstLit(v: String) extends Literal
case class SymbolLit(v: String) extends Literal
case class StringLit(v: String) extends Literal
case class LVar(val v: String) extends Literal
case class IVar(v: String) extends Literal
case class ATToken(v: String) extends Literal // @token
object ATToken {
  def apply(l: LVar) = new ATToken(l.v)
}

case class Keyword(v: String) extends Literal

sealed trait Expr extends AST
case class ARef(v: Expr, ref: Expr) extends Expr
case class Ary(v: List[Expr]) extends Expr
case class Hash(v: Map[Expr, Expr]) extends Expr
case class IfExpr(cond: Expr, tBody: Stmnts, elsifBody: List[ElsifBody], fBody: Option[Stmnts]) extends Expr
case class ElsifBody(cond: Expr, body: Stmnts)

case class IfModExpr(cond: Expr, expr: Expr) extends Expr
case class UnlessExpr(cond: Expr, t_body: Stmnts, f_body: Option[Stmnts]) extends Expr
case class UnlessModExpr(cond: Expr, t_body: Expr) extends Expr
case class Return(args: List[Expr]) extends Expr
case class Unary(op: Op, v: Expr) extends Expr
case class Binary(v: Op, lht: Expr, rht: Expr) extends Expr
case class Call(rev: Option[Expr], name: String, args: Option[ActualArgs], block: Option[Block]) extends Expr
case class Cmd(rev: Option[Expr], name: String, args: Option[ActualArgs], block: Option[Block]) extends Expr
case class Assign(target: Expr, value: Expr, op: Op) extends Expr
case class ClassExpr(name: ConstLit, parent: Option[Expr], body: Stmnts) extends Expr
case class ModuleExpr(name: ConstLit, body: Stmnts) extends Expr
case class DefExpr(name: String, args: Option[FormalArgs], body: Stmnts) extends Expr

sealed abstract class Block(args: Option[ActualArgs], body: Stmnts) extends Expr
case class DoBlock(args: Option[ActualArgs], body: Stmnts) extends Block(args, body)
case class BraceBlock(args: Option[ActualArgs], body: Stmnts) extends Block(args, body)

case class Stmnts(v: List[Expr]) extends AST {
  def foreach = v.foreach _
  def map(f: Expr => Expr): Stmnts = Stmnts(v.map(f))
  def prependExpr(e: Option[Expr]) = Stmnts(e.map(_ :: v).getOrElse(v))
}

object Operators  {
  def apply(tags: Set[String], defis: Seq[(Syntax, Stmnts)]): Operators =
    Operators(defis.map { case (syn, smn) => Operator(tags, syn, smn) })
}

case class Operators(ops: Seq[Operator]) extends Expr {
  def foreach = ops.foreach _
}
case class Operator(tags: Set[String], syntax: Syntax, body: Stmnts) extends Expr with MethodTranslate {
  val syntaxBody: List[String] = syntax.body
  val syntaxTags: Map[String, Expr] = syntax.tags

  def terms: Seq[Either[(String, Expr), String]] = syntax.body.map { term =>
    syntaxTags.get(term) match {
      case Some(expr) => Left((term, expr))
      case None => Right(term)
    }
  }

  def hasToken(token: String) = syntax.terminals.contains(token)
}

case class Syntax(tags: Map[String, Expr], body: List[String]) extends Expr {
  lazy val terminals = body.filter(!_.contains(nonTerminals))
  lazy val nonTerminals = tags.keys
}
