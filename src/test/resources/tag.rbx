Operator(lang_name, object, dynamic)
  defs ruby ()
    "ruby-lang"
  end
end

Operator(lang_name, object, static)
  defs java ()
    "java-lang"
  end
end

Operator(lang_name, functional, static)
  defs haskell ()
     "haskell-lang"
  end

  defs scala ()
     "scala-lang"
  end
end

Operator(lang_name, functional, dynamic)
  defs clojure ()
     "clojure-lang"
  end
end

Operator(origin)
  defs aprint arg1 and arg2(arg1: lang_name && dynamic ,arg2: lang_name && !object)
     puts arg1 + " and " + arg2 + " are awesome"
  end
end

aprint ruby and haskell