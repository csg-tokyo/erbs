package rbparser

import org.scalatest._
import scala.io.Source
import java.io._
import scala.sys.process._
import rbparser.parser.ast._

class IntegrationTest extends FunSpec {
  describe ("Pretty Print") {
    it ("should return ruby soruce code") {
      fileAssert("basic_class.rbx")
      fileAssert("complex_pp_test.rbx")
    }
  }

  describe ("Extend Parser") {
    it ("should return value of ruby") {
      fileAssertEqual("cat.rbx", "cat file_path")
      fileAssertEqual("cat_ruby.rbx", "cat rack.rb")
      fileAssertEqual("aws_provider.rbx", "provider aws { access_key = your acess key }")
      fileAssertEqual("recurcive_define.rbx", "provider id1 id2 id1 id1")
    }
  }

  private def fileAssertEqual(filename: String, v: Any) = {
    val body = loadFile(filename)
    val ppBody = translate(body)
    assertResult(v) {
      TmpFile.open(".rb") { file =>
        file.write(ppBody)
        val t = Process(s"ruby ${file.name}").!!;
        t.trim
      }
    }
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

  object TmpFile {
    def open(ext: String)(fun: TmpFile => Any): Any = {
      val t = new TmpFile(ext)
      try { fun(t) } finally { t.destroy }
    }
  }

  class TmpFile(ext: String) {
    private lazy val file: File = new File(name)

    lazy val name = "_TMP_" + System.currentTimeMillis + ext

    def write(data: String) = {
      val writer = new FileWriter(file)

      try {
        writer.write(data)
      } catch {
        case e:Exception => throw e
      } finally {
        writer.close
      }
    }

    def destroy = file.delete
  }
}
