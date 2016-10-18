Operator(file)
  defs f(f: origin)
    f
  end
end

Operator(origin)
  defs cat filename > output (filename: file && !@token(>), output: origin)
   puts "redirect " + filename + " to " + output
  end
end

cat "build.sbt" > "tmp"
