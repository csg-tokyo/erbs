package rbparser.parser.util

import scala.util.parsing.combinator.PackratParsers
import scala.collection.mutable.{Map => MMap}

trait MapUtil extends PackratParsers {
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
}
