Operator(provider, oneline)
 defs access_key = e (e: origin)
   "  access_key = " + e + "\n"
 end

 defs secret_key = e (e: origin)
   "  secret_key = " + e + "\n"
 end

 defs region = e (e: origin)
   "  region= " + e + "\n"
 end
end

Operator(config_body, provider)
  defs a (a: provider && oneline)
    a
  end

  defs a b (a: provider && oneline, b: config_body && provider)
    a + b
  end
end

Operator(config, provider)
  defs aws { body } (body: provider && config_body)
    "aws " + "{\n" + body + "}"
  end
end

Operator(origin, provider)
  defs provider config (config: provider && config)
    puts("provider " + config)
  end
end

provider aws {
  access_key = "your acess key"
  secret_key = "your secret key"
  region =  "us-east-1"
}
