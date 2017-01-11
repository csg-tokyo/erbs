Operator(origin)
  defs cond ? t : f (cond: origin, t: origin, f: origin)
    if  cond
      t
    else
      f
    end
  end
end

ret = true ? "true" : "false"
puts ret
