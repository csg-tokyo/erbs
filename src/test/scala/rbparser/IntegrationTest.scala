package rbparser

import org.scalatest._
import scala.io.Source
import rbparser.parser.ast._

class IntegrationTest extends FunSpec {
  it ("foo primitive") {
    fileAssert("basic_class.rb")
  }

  private def fileAssert(filename: String) = {
    val body = loadFile(filename)
    val ppBody = translate(body)
    assert(body.trim == ppBody.trim)
  }

  private def translate(source: String): String = PrettyPrinter.call(parse(source))

  private def loadFile(filename: String): String = {
    val source = Source.fromFile("src/test/resources/"+filename)
    val lines = source.getLines.toList
    lines.mkString("\n")
  }

  private def parse(x: String): AST = {
    val parser = new Parser()
    parser.parse(x + "\n") match {
      case Right(x) => x
      case Left(s) => throw new Exception(s)
    }
  }
}
