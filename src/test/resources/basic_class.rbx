class Sample
  attr_reader :foo, :bar, :val

  def load(file_name)
    puts("load:" + file_name)
  end

  def baz
    @baz ||= 123 * 100
  end

  def bar
    @foo = a.foo
    @bar = b.b("bar")
    10 < c.call(10)
  end

  def method_name
    run :dry do |x|
      x + 1
    end
  end
end

s = Sample.new
s.load('ruby file')
