import rbparser._

object Main {
  def main(args: Array[String]) {

    println("nothing to do")

    val p = new Parser
    // println(p.parse("a[]\n"))
    println(p.parse("@a.call + 10\n"))
    // println(p.parse("!a1._calL?(10)\n"))
    // println(p.parse("!a1.call(10,10)\n"))
    // println(p.parse("!(true && false)"))
    // println(p.parse("a1.call(10,10)\n"))
    // println(p.parse("!a1.a.call(10)\n"))
    // println(p.parse("!true && false\n"))
    // println(p.parse("@a[i] = 2\n"))
  }
}
