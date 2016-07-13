Operator(file_name)
  defs name (name: origin)
    name + ".rb"
  end

end

Operator(origin)
  defs cat file_name (file_name: file_name)
    puts("cat " + file_name)
  end
end

cat "rack"
