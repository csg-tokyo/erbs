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

  object Hoge {
    def apply[T](op: Operator, p: => PackratParser[T]) = new Hoge(List(op), List(() => p))
  }

  case class Hoge[T](val operators: List[Operator], parsers: List[() => PackratParser[T]]) {
    def isEmpty = operators.isEmpty || parsers.isEmpty

    // TODO Should be able to receive multiple tokens
    def selectByToken(token: String): Hoge[T] = {
      val (o,p) = operators.zip(parsers).filter { case (o, p) => o.hasToken(token) }.unzip
      Hoge(o, p)
    }

    def selectByContext(context: Context): Hoge[T] = {
      val (o,p) = operators.zip(parsers).filter { case (o, p) =>
        context.ok.forall(o.hasToken(_)) && context.ng.forall(!o.hasToken(_))
      }.unzip
      Hoge(o, p)
    }

    def renew(op: Operator, p: => PackratParser[T]) = Hoge(op :: operators , (() => p) :: parsers)

    def toParser: Option[PackratParser[T]] =
      parsers.reduceLeftOption { (acc, v) => () => acc() | v() }.map(_())
  }

  type Tag = String
  type Tags = Set[Tag]

  class HogeMap[T](storage: MMap[Tags, Hoge[T]] = MMap.empty[Tags, Hoge[T]]) {
    // TODO implement "bulk save"

    def put(tags: Tags, op: Operator, p: => PackratParser[T]) =
      storage.get(tags) match {
        case None =>  storage.put(tags, Hoge(op, p))
        case Some(h) => storage.put(tags, h.renew(op, p))
      }

    def get(ts: Tag) = searchBy(_.contains(ts))

    def getNot(ts: Tag) = searchBy(!_.contains(ts))

    def getWithAllMatch(ts: Tags) = searchBy(ts.subsetOf(_))

    def getWithAllMatch(ts: Tags, exceptKey: Tags) = searchBy {
      tags => ts.subsetOf(tags) && exceptKey.forall(!tags.contains(_))
    }

    def getParsers(contxt: Context, ts: Tags, exceptTags: Tags): Iterable[Hoge[T]] =
      getParsers(ts, exceptTags).map { _.selectByContext(contxt) }.filter(!_.isEmpty)

    def getParsers(ts: Tags, exceptTags: Tags): Iterable[Hoge[T]] =
      storage.filterKeys{ tags => ts.subsetOf(tags) && exceptTags.forall(!tags.contains(_)) }.values

    def toModule: Option[ModuleExpr] = if (storage.size == 0) None else {
      val opss = storage.values.flatMap(_.operators)
      val body = opss.groupBy(_.className).map { case (k, v) =>
        ClassExpr(ConstLit(k), Stmnts(v.map(_.toMethod).toList))
      }
      Some(ModuleExpr(ConstLit("Operator"), Stmnts(body.toList)))
    }

    private def searchBy(cond: Tags => Boolean): Option[PackratParser[T]] =
      storage.filterKeys(cond).values.flatMap(_.toParser).reduceLeftOption { (acc, v) => acc | v }
  }

  // def rebuild(k: Tags, exceptKey: Tags, c: String) = {
  //   val hs = getParsers(k, exceptKey).map { _.selectByToken(c) }.filter(!_.isEmpty)
  //   for (h <- hs) { h.rebuild(c) }
  // }
}
