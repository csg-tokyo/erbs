package erbs
package parser

import util.{EmptySet}
import ast.{Expr, ATToken, Unary,EXT, Binary, AND}

case class Context(val ok: Set[String] = Set(), val ng: Set[String] = Set()) {
  lazy val isEmpty = ok.isEmpty && ng.isEmpty

  lazy val contextSeq: Seq[Expr] =
    ok.toSeq.map(ATToken(_)) ++ ng.toSeq.map { n => Unary(EXT, ATToken(n)) }

  lazy val contextExpr: Option[Expr] =
    if (isEmpty) None else Some(contextSeq.reduceLeft{ (acc, e) => Binary(AND, acc, e) })

  def fold[T](default: T)(fn: Expr => T) = contextExpr.fold(default)(fn(_))

  def cloneWith(c: (Set[String], Set[String])) = c match {
    case (nok, nng) =>  Context(nok ++ ok, nng ++ ng)
  }

  def collect[T](m: Map[String, T]): Map[String, T] = (ok, ng) match {
    case (EmptySet(), n) => m.filter(e => !n.contains(e._1))
    case (o, EmptySet()) => m.filter(e => o.contains(e._1))
    case (o, n) => m.filter(e => !n.contains(e._1)).filter(e => o.contains(e._1))
    case _ => Map()
  }
}
