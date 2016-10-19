Operator(foo)
  defs xx $ (xx: origin)
    xx
  end
end

Operator(mod, origin)
  defs x -> y (x: foo && !@token(->))
    puts x
  end
end

10 $ -> y
