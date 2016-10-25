Operator(terraform, docker_provider)
  defs host = e (e: origin)
    "host = " + e
  end
end

Operator(terraform, provider)
  defs docker { body } (body: terraform && docker_provider)
    "docker " + "{ " + body + " }"
  end
end

Operator(origin, terraform)
  defs provider configs (configs: terraform && provider)
    puts("provider " + configs)
  end
end

Operator(terraform, docker_resource)
  defs image = path (path: origin)
    " access_key = " + path
  end

  defs name = n (n: origin)
    " secret_key = " + n
  end
end

Operator(terraform, docker_resource_multiple)
  defs a (a: terraform && docker_resource)
    a
  end

  defs a b (a: terraform && docker_resource, b: terraform && docker_resource_multiple)
    a + b
  end
end

Operator(terraform, resource)
  defs docker_container name { body } (name: origin, body: terraform && docker_resource_multiple)
    "docker_container '" + name + "' { " + body + " }"
  end

  defs docker_image name { body } (name: origin, body: terraform && docker_resource_multiple)
    "docker_image '" + name + "' { " + body + " }"
  end
end

Operator(origin, resource)
  defs resource configs (configs: terraform && resource)
    puts("provider " + configs)
  end
end

provider docker {
  host = "tcp://127.0.0.1:2376/"
}

resource docker_container "foo" {
  image = "${docker_image.ubuntu.latest}"
  name = "foo"
}

resource docker_image "ubuntu" {
  name = "ubuntu:latest"
}
