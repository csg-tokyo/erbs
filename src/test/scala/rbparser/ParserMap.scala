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

  def assertParseResult(in: String)(p: MyParsers.PackratParser[Char])(parser: Option[A.PackratParser[Char]]) =
    assertResult(parser.map { parseAll(_, in).get }) { Some(p).map { x => (MyParsers.phrase(x)(new CharSequenceReader(in))).get } }

  describe("#get") {
    val m = ParserMap.empty[String, Char]
    m.put(Set("origin", "add"), add)
    m.put(Set("origin", "sub"), sub)
    m.put(Set("mul"), mul)

    it ("returns specifed value") {
      m.get("add") shouldBe Some(add)
      m.get("mul") shouldBe Some(mul)
      m.get("foo") shouldBe None
    }

    def assertParseResult(tag: String, in: String, p: MyParsers.PackratParser[Char]) = assertResult(m.get(tag).map { parseAll(_, in).get }) {
      Some(p).map { x => (MyParsers.phrase(x)(new CharSequenceReader(in))).get }
  describe("#getNot") {
    val m = ParserMap.empty[String, Char]
    m.put(Set("origin", "add"), add)
    m.put(Set("origin", "sub"), sub)
    m.put(Set("mul", "sub"), mul)

    it ("returns specifed value") {
      assertResult(Some(mul)) { m.getNot("origin") }
      assertResult(Some(add)) { m.getNot("sub") }
    }

    it ("returns mutiple Parsers") {
      assertParseResult("-") { sub | mul } { m.getNot("add") }
      assertParseResult("*") { sub | mul } { m.getNot("add") }

      withClue("does not returns if can't parse") {
        intercept[java.lang.RuntimeException] {
          assertParseResult("+") { sub | mul } { m.getNot("add") }
        }
      }
    }
  }

  describe("#getWithAllMatch") {
    val m = ParserMap.empty[String, Char]
    m.put(Set("origin", "add"), add)
    m.put(Set("origin", "sub"), sub)
    m.put(Set("origin", "sub", "mul"), div)
    m.put(Set("mul"), mul)
    m.put(Set("div"), div)

    it ("returns specifed value") {
      assertResult(Some(add)) { m.getWithAllMatch(Set("add", "origin")) }
      assertResult(Some(div)) { m.getWithAllMatch(Set("div")) }
      assertResult(None) { m.getWithAllMatch(Set("mul", "foo")) }
    }

    def assertParseResult(tag: Set[String], in: String, p: MyParsers.PackratParser[Char]) = assertResult(m.getWithAllMatch(tag).map { parseAll(_, in).get }) {
      Some(p).map { x => (MyParsers.phrase(x)(new CharSequenceReader(in))).get }
    }

    it ("returns mutiple Parsers") {
      assertParseResult(Set("origin", "sub"), "-", sub | div)
      assertParseResult(Set("origin", "sub"), "/", sub | div)

      withClue("does not returns if can't parse") {
        intercept[java.lang.RuntimeException] {
          assertParseResult(Set("origin", "sub"), "&", sub | div)
        }
      }
    }
  }
}
