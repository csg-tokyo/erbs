package rbparser

import org.scalatest._
import Matchers._

import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader
import parser.ExtendableParser
import scala.language.implicitConversions

class ParserMapTest extends FunSpec {
  // Hack: A exnteds extendableParser and import A._ to test ParserMap
  object A extends ExtendableParser
  import A._

  object MyParsers extends PackratParsers {
    type Elem = Char
    lazy val add: PackratParser[Elem] = elem('+')
    lazy val sub: PackratParser[Elem] = elem('-')
    lazy val mul: PackratParser[Elem] = elem('*')
    lazy val div: PackratParser[Elem] = elem('/')

  }

  import MyParsers._

  // cast implicitly for testing
  implicit def toAcceptableParser(p: MyParsers.PackratParser[Char]): A.PackratParser[Char] = p.asInstanceOf[A.PackratParser[Char]]

  describe("#get") {
    val m = ParserMap.empty[String, Char]
    m.put(List("origin", "add"), add)
    m.put(List("origin", "sub"), sub)
    m.put(List("mul"), mul)

    it ("returns specifed value") {
      m.get("add") shouldBe Some(add)
      m.get("mul") shouldBe Some(mul)
      m.get("foo") shouldBe None
    }

    def assertParseResult(tag: String, in: String, p: MyParsers.PackratParser[Char]) = assertResult(m.get(tag).map { parseAll(_, in).get }) {
      Some(p).map { x => (MyParsers.phrase(x)(new CharSequenceReader(in))).get }
    }

    it ("returns mutiple Parsers") {
      assertParseResult("origin", "+", add | sub)
      assertParseResult("origin", "-", add | sub)

      withClue("does not returns if can't parse") {
        intercept[java.lang.RuntimeException] {
          assertParseResult("origin", "&", add | sub)
        }
      }
    }
  }

  describe("#getWithAllMatch") {
    val m = ParserMap.empty[String, Char]
    m.put(List("origin", "add"), add)
    m.put(List("origin", "sub"), sub)
    m.put(List("origin", "sub", "mul"), div)
    m.put(List("mul"), mul)
    m.put(List("div"), div)

    it ("returns specifed value") {
      assertResult(Some(add)) { m.getWithAllMatch(List("add", "origin")) }
      assertResult(Some(div)) { m.getWithAllMatch(List("div")) }
      assertResult(None) { m.getWithAllMatch(List("mul", "foo")) }
    }

    def assertParseResult(tag: List[String], in: String, p: MyParsers.PackratParser[Char]) = assertResult(m.getWithAllMatch(tag).map { parseAll(_, in).get }) {
      Some(p).map { x => (MyParsers.phrase(x)(new CharSequenceReader(in))).get }
    }

    it ("returns mutiple Parsers") {
      assertParseResult(List("origin", "sub"), "-", sub | div)
      assertParseResult(List("origin", "sub"), "/", sub | div)

      withClue("does not returns if can't parse") {
        intercept[java.lang.RuntimeException] {
          assertParseResult(List("origin", "sub"), "&", sub | div)
        }
      }
    }
  }
}
