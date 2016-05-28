package rbparser

object PrettyPrinter {
  def print(ast: ASTs) = {
    println(PrettyPrinter(ast, 0).call)
  }
}

private[rbparser] case class PrettyPrinter(ast: ASTs, private var depth: Int) {
  val buffer = new StringBuilder

  def call: String = {
    _write(ast)
    flush()
  }

  def _write(ast: ASTs) = ast match {
    case literal: Literal[_] => write(literal.toString)
    case Binary(op, lhs, rhs) => {
      write(format(lhs) + s" ${Op.stringfy(op)} " + format(rhs))
    }
    case Assign(lhs, rhs, op) => {
      write(format(lhs) + s" ${Op.stringfy(op)} " + format(rhs))
    }
    case IfExpr(cond, Stmnts(stmnts)) => {
      writeln("if "+format(cond))
      nested { stmnts.foreach { stmnt => indented_write(format(stmnt)+"\n") } }
      indented_write("end")
    }
    case ClassExpr(name, Stmnts(stmnts)) => {
      writeln("class "+ format(name))
      nested {
        for (i <- 0 until stmnts.length) {
          val cr = if (i == stmnts.length-1) "\n"  else "\n\n"
          indented_write(format(stmnts(i))+cr)
        }
      }
      indented_write("end")
    }
    case DefExpr(name, args, Stmnts(stmnts)) => {
      writeln("def "+ name + args.fold("") ("("+format(_)+")"))
      nested { stmnts.foreach { stmnt => indented_write(format(stmnt)+"\n") } }
      indented_write("end")
    }
    case FormalArgs(vars) => vars match {
      case Nil => ()
      case _ => writeEachWithComma(vars)
    }
    case Stmnts(stmnts) => stmnts.foreach { x => writeln(format(x)) }
  }

  private def flush(): String = buffer.toString()

  private def write(text: String) = buffer.append(text)

  private def writeln(text: String) = write(text+"\n")

  private def indented_write(txt: String) = write(indented+txt)

  private def indented: String = "  " * depth

  private def nested(fn: => Unit) = {
    depth += 1
    fn
    depth -= 1
  }

  private def format(ast: ASTs): String = PrettyPrinter(ast, depth).call

  private def writeEachWithComma(vars: List[Expr]) = write(vars.map(format(_)).mkString(", "))

  // def format(ast: ASTs): String = ast match {
  //   case literal: Literal[_] => literal.toString()
  //   case Ary(lst) => s"[${joinWithComma(lst)}]"
  //   case Binary(op, lhs, rhs) => s"${format(lhs)} ${Op.stringfy(op)} ${format(rhs)}"
  //   case Assign(lhs, rhs, op) => s"${format(lhs)} ${Op.stringfy(op)} ${format(rhs)}"
  //   case Call(rev, name, arg, block) => s"""${rev.fold("")(format(_)+".")}$name${arg.fold("")("(" + format(_) + ")")}${block.fold("")(format(_))}"""
  //   case Cmd(rev, name, arg, block) => s"""${rev.fold("")(format(_)+".")}$name${arg.fold("")(" "+format(_))}${block.fold("")(format(_))}"""
  //   case DoBlock(args, body) => s" do${args.fold("") (e => " |" + format(e) + "|")}\n${format(body)}\nend"
  //   case BraceBlock(args, body) => s" { ${args.fold("") (e => "|" + format(e) + "| ")}${format(body)} }"
  //   case ARef(v, ref) => s"${format(v)}[${format(ref)}]"
  //   case IfExpr(cond, expr) => s"if ${format(cond)}\n${format(expr)}\nend"
  //   case UnlessExpr(cond, expr) => s"unless ${format(cond)}\n${format(expr)}\nend"
  //   case IfModExpr(cond, expr) => s"${format(expr)} if ${format(cond)}"
  //   case UnlessModExpr(cond, expr) => s"${format(expr)} unless ${format(cond)}"
  //   case Return(args) => s"return ${joinWithComma(args)}".stripSuffix(" ")
  //   case ActualArgs(names) => joinWithComma(names)
  //   case FormalArgs(names) => names match {
  //     case Nil => ""
  //     case x => s"(${joinWithComma(x)})"
  //   }
  //   case Unary(op, expr) => Op.stringfy(op) + (if (expr.isInstanceOf[Binary]) s"(${format(expr)})" else format(expr))
  //   case ClassExpr(name, stmnts) => {
  //     val s = format(stmnts)
  //     s"class ${format(name)}" + "\n" + (if (s == "") "" else s+"\n") + "end"
  //   }
  //   case DefExpr(name, args, stmnts) => {
  //     val s = format(stmnts)
  //     s"def $name${args.fold("")(format(_))}\n" + (if (s == "") "" else s+"\n") + "end"
  //   }
  //   case Stmnts(v) => v.map(format(_)).mkString("\n")
  // }

  // private def joinWithComma(args: List[Expr]) = args.map(x => format(x)).mkString(", ")

}
