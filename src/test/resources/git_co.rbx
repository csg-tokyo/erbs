Operator(git, subcommand)
  defs checkout branch_name (branch_name: origin)
    "checkout " + branch_name
  end
end

Operator(origin, git, command)
  defs git scmd (scmd: git && subcommand)
    puts "git " + scmd
  end
end

git checkout "master"
