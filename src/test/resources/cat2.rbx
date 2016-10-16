Operator(origin)
  defs cat file filea (file: origin, filea: origin)
     puts "contents of " + file
     puts "contents of " + filea
  end
end

cat "hello.c" "hello2.c"