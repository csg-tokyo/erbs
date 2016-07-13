import rbparser._

object Main {
  def main(args: Array[String]): Unit = {
    // test1
  }

  def test1 = {
    val p = new Parser
    println(p.parse("@a.call + 10\n"))
    println(p.parse("!a1._calL?(10)\n"))
    println(p.parse("!a1.call(10,10)\n"))
    println(p.parse("!a1.a.call(10)\n"))
  }
}
