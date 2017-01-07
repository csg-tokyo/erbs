Operator(command, origin)
  defs cat filename(filename: origin)
    $pipe = File.read(filename).split("\n")
  end

  defs grep pat (pat: origin)
     $pipe = $pipe.select { |f| f.include?(pat) }
  end
end

Operator(origin)
  defs cmd1 | cmd2 (cmd1: command && !@token(|), cmd2: origin)
    cmd1
    cmd2
    puts $pipe
  end
end

cat ".gitignore" | grep "ensime_cache"
