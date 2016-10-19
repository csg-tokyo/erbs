Operator(command)
  defs cat f1 f2 (f1: origin, f2: origin)
     f1 + f2
  end
end

Operator(origin)
  defs x > y (x: command && !@token(>), y: origin)
    puts x + y
  end
end

cat "abcd" "efgh" > "ijkl"
