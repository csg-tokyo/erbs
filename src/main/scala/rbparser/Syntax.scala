package rbparser

sealed trait Syntax
sealed trait Literal extends Syntax

case class DoubleLit(v: Double) extends Literal
case class IntLit(v: Int) extends Literal
case class BoolLit(v: Boolean) extends Literal
case class IdLit(v: String) extends Literal

case class Prog(v: List[Syntax]) extends Syntax
