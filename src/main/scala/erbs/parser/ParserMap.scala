package erbs.parser

import scala.util.parsing.combinator.PackratParsers
import scala.collection.mutable.{Map => MMap}
import erbs.parser.ast.{Operator, Stmnts, ClassExpr, ConstLit, ModuleExpr}

trait ParserMap extends PackratParsers {
  object ParserMap {
    def empty[T, S] = new ParserMap[T, S]
  }

  class ParserMap[T, S] (m: MMap[Set[T], () => PackratParser[S]] = MMap.empty[Set[T], () => PackratParser[S]]) {
    def get(k: T) = searchBy(_.contains(k))

    def getNot(k: T) = searchBy(!_.contains(k))

    def getWithAllMatch(k: Set[T]) = searchBy(k.subsetOf(_))

    def getWithAllMatch(k: Set[T], exceptKey: Set[T]) = searchBy { key => k.subsetOf(key) && exceptKey.forall { !key.contains(_) } }

    def put(key: Set[T], value: => PackratParser[S]) = m.get(key) match {
      case None => m.put(key, () => value)
      case Some(parser) => m.put(key, () => value | parser())
    }

    private def searchBy(cond: Set[T] => Boolean): Option[PackratParser[S]] =
      m.filterKeys(cond).values.reduceLeftOption { (acc, v) => () => acc() | v() }.map(_())
  }

  type Tag = String
  type Tags = Set[Tag]

  // This class represents an Erbs operator and its parser
  case class ErbsOperator[T](val operator: Operator, val parser: () => PackratParser[T]) {
    def inContext(context: Context) =
      context.ok.forall(operator.hasToken(_)) && context.ng.forall(!operator.hasToken(_))
  }

  class ErbsOpMap[T](storage: MMap[Tags, List[ErbsOperator[T]]] = MMap.empty[Tags, List[ErbsOperator[T]]]) {
    def put(tags: Tags, value: ErbsOperator[T]) =
      storage.get(tags) match {
        case None =>  storage.put(tags, List(value))
        case Some(items) => storage.put(tags, value :: items)
      }

    def get(ts: Tag) = searchBy(_.contains(ts))

    def getNot(ts: Tag) = searchBy(!_.contains(ts))

    def getWithAllMatch(ts: Tags) = searchBy(ts.subsetOf(_))

    def getWithAllMatch(ts: Tags, exceptKey: Tags) = searchBy {
      tags => ts.subsetOf(tags) && exceptKey.forall(!tags.contains(_))
    }

    private def searchBy(cond: Tags => Boolean): Option[PackratParser[T]] =
      storage.filterKeys(cond).values.flatMap { p => p.map(_.parser()) }.reduceLeftOption { (acc, v) => acc | v }

    def getParsers(context: Context, ts: Tags, exceptTags: Tags): Iterable[ErbsOperator[T]] =
      getParsers(ts, exceptTags).filter(_.inContext(context))

    def getParsers(ts: Tags, exceptTags: Tags): Iterable[ErbsOperator[T]] =
      storage.filterKeys { tags => ts.subsetOf(tags) && exceptTags.forall(!tags.contains(_)) }
        .values.flatMap(identity(_))

    def toModule: Option[ModuleExpr] = if (storage.size == 0) None else {
      val opss = storage.values.flatMap { v => v.map(_.operator) }
      val body = opss.groupBy(_.className).map { case (k, v) =>
        ClassExpr(ConstLit(k), None, Stmnts(v.map(_.toMethodDefinition).toList))
      }
      Some(ModuleExpr(ConstLit("Operator"), Stmnts(body.toList)))
    }
  }
}
