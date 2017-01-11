Operator(args)
  defs a (a: origin)
   a
  end

  defs a b (a: origin, b: args)
    a + b
  end
end

Operator(origin)
  defs sum argument (argument: args)
    argument
  end
end

sum 1 2 3 4 5 6 7 8 9 10
