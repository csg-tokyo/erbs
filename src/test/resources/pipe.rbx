Operator(origin)
  defs cat filename | grep pat (filename: origin, pat: origin)
    file = File.read(filename).split("\n").select { |f| f.include?(pat) }
    puts file
  end
end

cat ".gitignore" | grep "ensime_cache"
