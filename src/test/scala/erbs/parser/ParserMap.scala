package erbs.parser

import org.scalatest._

import scala.util.parsing.combinator.{PackratParsers}
import scala.util.parsing.input.CharSequenceReader
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
      assertResult(Some(add)) { m.get("add") }
      assertResult(Some(mul)) { m.get("mul") }
      assertResult(None)      { m.get("foo") }
    }

    it ("returns mutiple Parsers") {
      assertParseResult("+") { add | sub } { m.get("origin") }
      assertParseResult("-") { add | sub } { m.get("origin") }

      withClue("does not returns if can't parse") {
        intercept[java.lang.RuntimeException] {
          assertParseResult("&") { add | sub } { m.get("origin") }
        }
      }
    }
  }

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
    val m =  ParserMap.empty[String, Char]
    m.put(Set("origin", "add"), add)
    m.put(Set("origin", "sub"), sub)
    m.put(Set("origin", "sub", "mul"), div)
    m.put(Set("mul"), mul)
    m.put(Set("div"), div)

    it ("returns specifed value") {
      assertResult(Some(add)) { m.getWithAllMatch(Set("add", "origin")) }
      assertResult(Some(div)) { m.getWithAllMatch(Set("div")) }
      assertResult(None)      { m.getWithAllMatch(Set("mul", "foo")) }
    }

    it ("returns mutiple Parsers") {
      assertParseResult("-") { sub | div } { m.getWithAllMatch(Set("origin", "sub")) }
      assertParseResult("/") { sub | div } { m.getWithAllMatch(Set("origin", "sub")) }

      withClue("does not returns if can't parse") {
        intercept[java.lang.RuntimeException] {
          assertParseResult("&") { sub | div } { m.getWithAllMatch(Set("origin", "sub")) }
        }
      }
    }
  }

  describe("#getWithAllMatch with execept key") {
    val m = ParserMap.empty[String, Char]
    m.put(Set("origin", "sub"), sub)
    m.put(Set("origin", "sub", "mul"), div)
    m.put(Set("origin", "add", "mul"), add)
    m.put(Set("origin", "add"), mul)

    it ("returns specifed value") {
      assertResult(Some(sub)) { m.getWithAllMatch(Set("origin", "sub"), Set("mul")) }
      assertResult(Some(mul)) { m.getWithAllMatch(Set("origin", "add"), Set("mul")) }
      assertResult(None) { m.getWithAllMatch(Set("origin", "add"), Set("mul", "add")) }
    }

    it ("returns mutiple Parsers") {
      assertParseResult("-") { sub | mul } { m.getWithAllMatch(Set("origin"), Set("mul")) }
      assertParseResult("*") { sub | mul } { m.getWithAllMatch(Set("origin"), Set("mul")) }

      withClue("does not returns if can't parse") {
        intercept[java.lang.RuntimeException] {
          assertParseResult("/") { sub | mul } { m.getWithAllMatch(Set("origin"), Set("mul")) }
        }
      }
    }
  }
}
