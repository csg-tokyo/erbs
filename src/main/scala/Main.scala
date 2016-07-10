import rbparser._

object Main {
  def main(args: Array[String]) {
    // test1
    // test2
    // test3
    test4
  }

  def test4 = {
    val p = new Parser
    val v = p.parse("""
Operator(provider, oneline)
 defs access_key = e (e: origin)
   "  access_key = " + e + "\n"
 end

 defs secret_key = e (e: origin)
   "  secret_key = " + e + "\n"
 end

 defs region = e (e: origin)
   "  region= " + e + "\n"
 end
end

Operator(config_body, provider)
  defs a b (a: provider && oneline, b: config_body && provider)
    a + b
  end

  defs a (a: provider && oneline)
    a
  end
end

Operator(config, provider)
  defs aws { body } (body: provider && config_body)
    "aws " + "{\n" + body + "}"
  end
end

Operator(origin, provider)
  defs provider config (config: provider && config)
    puts("provider " + config)
  end
end

provider aws {
  access_key = "your acess key"
  secret_key = "your secret key"
  region =  "us-east-1"
}
""") match {
      case Right(x) => x
      case Left(x) =>
        println(x)
        throw new Exception
    }

    println(v)

    PrettyPrinter.print(v)
  }


  def test3 = {
    val p = new Parser
    val v = p.parse("""
Operator(name)
  defs ec2_instance()
    "ec2_instance"
  end

  defs eip()
    "eip"
  end
end

Operator(origin, resource)
  defs resources e (e: name)
    puts("resources" + e)
  end
end

resources ec2_instance
""") match {
      case Right(x) => x
      case Left(x) =>
        println(x)
        throw new Exception
    }

    PrettyPrinter.print(v)
  }

  def test2 = {
    val p = new Parser
    val v = p.parse("""
    class A
      def a(a, b)
        a = [1, 2, 3] # asf
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
      case _ => throw new Exception
    }
    PrettyPrinter.print(v)
  }

  def test1 = {
    val p = new Parser
    println(p.parse("@a.call + 10\n"))
    println(p.parse("a (1)+1\n"))
    println(p.parse("a.a(1) < 1\n"))
    println(p.parse("a\n"))
    println(p.parse("@a.call + 10\n"))
    println(p.parse("!a1._calL?(10)\n"))
    println(p.parse("!a1.call(10,10)\n"))
    println(p.parse("!(true && false)"))
    println(p.parse("a1.call(10,10)\n"))
    println(p.parse("!a1.a.call(10)\n"))
    println(p.parse("!true && false\n"))
    println(p.parse("@a[i] = 2\n"))
  }
}
