package erbs
package parser

import erbs.parser.ast.{Expr, ATToken, Unary,EXT, Binary, AND}

case class Context(
  val ok: Set[String] = Set(),
  val ng: Set[String] = Set()
) {
  lazy val isEmpty = ok.isEmpty && ng.isEmpty

  lazy val contextSeq: Seq[Expr] =
    ok.toSeq.map(ATToken(_)) ++ ng.toSeq.map { n => Unary(EXT, ATToken(n)) }

  lazy val contextExpr: Option[Expr] =
    if (isEmpty) None else Some(contextSeq.reduceLeft{ (acc, e) => Binary(AND, acc, e) })

  def fold[T](default: T)(fn: Expr => T) = contextExpr match {
      case None => default
      case Some(c) => fn(c)
    }

  def cloneWith(c: (Set[String], Set[String])) = c match {
    case (nok, nng) =>  Context(nok ++ ok, nng ++ ng)
  }
}
