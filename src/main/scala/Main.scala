import rbparser._

object Main {
  def main(args: Array[String]) {

    println("nothing to do")

    val p = new Parser
    println(p.parse("1 + 2 * 1 + 10\n"))
  }
}
