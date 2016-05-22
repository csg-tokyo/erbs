package rbparser

object PrettyPrinter {
  def call(p: ASTs): String = p match {
    case literal: Literal[_] => literal.toString()
    case Ary(lst) => s"[${joinWithComma(lst)}]"
    case Binary(op, lhs, rhs) => s"${call(lhs)} ${Op.stringfy(op)} ${call(rhs)}"
    case Assign(lhs, rhs, op) => s"${call(lhs)} ${Op.stringfy(op)} ${call(rhs)}"
    case Call(rev, name, arg, block) => s"""${rev.fold("")(call(_)+".")}$name${arg.fold("")("(" + call(_) + ")")}${block.fold("")(call(_))}"""
    case Cmd(rev, name, arg, block) => s"""${rev.fold("")(call(_)+".")}$name${arg.fold("")(" "+call(_))}${block.fold("")(call(_))}"""
    case DoBlock(args, body) => s" do${args.fold("") (e => " |" + call(e) + "|")}\n${call(body)}\nend"
    case BraceBlock(args, body) => s" { ${args.fold("") (e => "|" + call(e) + "| ")}${call(body)} }"
    case ARef(v, ref) => s"${call(v)}[${call(ref)}]"
    case IfExpr(cond, expr) => s"if ${call(cond)}\n${call(expr)}\nend"
    case UnlessExpr(cond, expr) => s"unless ${call(cond)}\n${call(expr)}\nend"
    case IfModExpr(cond, expr) => s"${call(expr)} if ${call(cond)}"
    case UnlessModExpr(cond, expr) => s"${call(expr)} unless ${call(cond)}"
    case Return(args) => s"return ${joinWithComma(args)}".stripSuffix(" ")
    case ActualArgs(names) => joinWithComma(names)
    case FormalArgs(names) => names match {
      case Nil => ""
      case x => s"(${joinWithComma(x)})"
    }
    case Unary(op, expr) => Op.stringfy(op) + (if (expr.isInstanceOf[Binary]) s"(${call(expr)})" else call(expr))
    case ClassExpr(name, stmnts) => {
      val s = call(stmnts)
      s"class ${call(name)}" + "\n" + (if (s == "") "" else s+"\n") + "end"
    }
    case DefExpr(name, args, stmnts) => {
      val s = call(stmnts)
      s"def $name${args.fold("")(call(_))}\n" + (if (s == "") "" else s+"\n") + "end"
    }
    case Stmnts(v) => v.map(call(_)).mkString("\n")
  }

  private def joinWithComma(args: List[Expr]) = args.map(x => call(x)).mkString(", ")
}
