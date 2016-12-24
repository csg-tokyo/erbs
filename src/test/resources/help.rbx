Operator(git, diff, subcommand)
 defs checkout bname(bname: origin)
  "checkout " + bname
 end
end

Operator(diff, option)
  defs --help ()
    "--help"
  end
end

Operator(diff, origin)
  defs diff cmd (cmd: diff && (subcommand || option))
     puts "diff " + cmd
  end
end

Operator(git, option)
  defs --help ()
    "--help"
  end
end

Operator(git, origin)
  defs git cmd (cmd: git && option)
    puts "git " + cmd
  end
end

diff --help
git --help
