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
    a += -1
    a.each(x) { |x| puts x + 1 }
    a = 2 + -(2 + 2)
    a.each { |x|
      puts x + 1
      puts y + 1
    }
  end
end
