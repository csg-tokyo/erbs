package rbparser

object PrettyPrinter {
  def call(p: ASTs): String = p match {
    case literal: Literal[_] => literal.toString()
    case Ary(None) => "[]"
    case Ary(Some(lst)) => "[" + lst.map(x => call(x)).mkString(", ") + "]"
    case Binary(op, lhs, rhs) => s"""${call(lhs)} ${Op.stringfy(op)} ${call(rhs)}"""
    case Assign(lhs, rhs) => s"""${call(lhs)} = ${call(rhs)}"""
    case Call(rev, name, arg, block) => s"""${rev.fold("")(x => call(x)+".")}$name${arg.fold("")("(" + call(_) + ")")}${block.fold("")(call(_))}"""
    case Cmd(rev, name, arg, block) => s"""${rev.fold("")(x => call(x)+".")}$name${arg.fold("")(" "+call(_))}${block.fold("")(call(_))}"""
    case DoBlock(args, body) => s""" do${args.fold("") (e => " |" + call(e) + "|")}"""+ "\n"+ call(body) + "\n" + "end"
    case BraceBlock(args, body) => s""" { ${args.fold("") (e => "|" + call(e) + "| ")}${call(body)} }"""
    case ARef(v, ref) => s"""${call(v)}[${call(ref)}]"""
    case IfExpr(cond, expr) => s"""if ${call(cond)}""" + "\n" + call(expr) + "\nend"
    case UnlessExpr(cond, expr) => s"""unless ${call(cond)}""" + "\n" + call(expr) + "\nend"
    case IfModExpr(cond, expr) => s"""${call(expr)} if ${call(cond)}"""
    case UnlessModExpr(cond, expr) => s"""${call(expr)} unless ${call(cond)}"""
    case Return(args) => args match { case Nil => "return"; case x => "return " + x.map(call(_)).mkString(", ") }
    case ActualArgs(names) => names.map(call(_)).mkString(", ")
    case Unary(op, expr) =>  expr match {
      case x: Binary => Op.stringfy(op) + s"""(${call(x)})""" // TO FIX
      case l => Op.stringfy(op) + call(l)
    }
    case ClassExpr(name, stmnts) => {
      val s = call(stmnts)
      s"""class ${call(name)}""" + "\n" + (if (s == "") "" else s+"\n") + "end"
    }
    case DefExpr(name, args, stmnts) => {
      val s = call(stmnts)
      "def " + name.toString() + args.fold("")(call(_)) + "\n" + (if (s == "") "" else s+"\n") + "end"
    }
    case FormalArgs(names) => names match {
      case Nil => ""
      case x => s"""(${x.map(call(_)).mkString(", ")})"""
    }
    case Stmnts(v) => v.map(call(_)).mkString("\n")

  }
}
