Operator(provider, oneline)
  defs access_key = e (e: origin)
    "access_key = " + e
  end
end


Operator(config, provider)
  defs aws { body } (body: provider && oneline)
    "aws " + "{ " + body + " }"
  end
end

Operator(origin, provider)
  defs provider config (config: provider && config)
    puts("provider " + config)
  end
end

provider aws {
  access_key = "your acess key"
}