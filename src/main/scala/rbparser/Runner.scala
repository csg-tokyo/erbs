package erbs

object Runner {
  def main(args: Array[String]): Unit = {
    // example1
  }

  def example1 = {
    val p = new Parser
    println(p.parse("@a.call + 10\n"))
    println(p.parse("!a1._calL?(10)\n"))
    println(p.parse("!a1.call(10,10)\n"))
    println(p.parse("!a1.a.call(10)\n"))
  }
}
