import rbparser._

object Main {
  def main(args: Array[String]) {

    // a() a () a(1) a (1) a(1,2) a (1,2)
    val p = new Parser
    val v = p.parse("""
class A
  def a(a, b)
    a = [1, 2, 3]
    b = 10 * 10 unless true
    k = a[0]
    call(10, 10)
    call 10
    call 1, 20
    b = 10 * 10 if true
  end

  def call
    k = if true
      a + 1
    end
    a = b + 1
    a + 2
    return a, b
  end

  def name
    methd do |x|
      puts x
    end

    methd :kk do
      puts x
    end
   a + -1
   a.each(x) { |x| puts x + 1}
   2 + -(2 + 2)
   a.each { |x|
     puts x + 1
     puts y + 1
   }

  end
end
""") match {
      case Right(x) => x
    }

    PrettyPrinter.print(v)

    // println()
    // println(p.parse("@a.call + 10\n"))
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
