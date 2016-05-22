import rbparser._

object Main {
  def main(args: Array[String]) {

    // a() a () a(1) a (1) a(1,2) a (1,2)
    val p = new Parser
    // println(p.parse("a(1)+1\n"))
    println(p.parse("[1,2].each\n"))
    // println(p.parse("a (1)+1\n"))
    // println(p.parse("a.a(1) < 1\n"))
    // println(p.parse("a\n"))
    // println(p.parse("@a.call + 10\n"))
    // println(p.parse("!a1._calL?(10)\n"))
    // println(p.parse("!a1.call(10,10)\n"))
    // println(p.parse("!(true && false)"))
    // println(p.parse("a1.call(10,10)\n"))
    // println(p.parse("!a1.a.call(10)\n"))
    // println(p.parse("!true && false\n"))
    // println(p.parse("@a[i] = 2\n"))
  }
}
