Operator(name)
  defs id1 ()
    "id1"
  end

  defs id2 ()
    "id2"
  end
end

Operator(config)
  defs one (one: name)
    one
  end

  defs one other (one: name, other: config)
    one + " " + other
  end
end

Operator(origin, provider)
  defs provider cc (cc: config)
    puts ("provider " + cc)
  end
end

provider id1 id2 id1 id1
