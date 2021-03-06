package erbs

import erbs.parser.ast._

object PrettyPrinter {
  def print(ast: AST) = println(call(ast))
  def call(ast: AST) = PrettyPrinter(ast, 0).call
}

case class PrettyPrinter(ast: AST, private var depth: Int) {
  private var classDepth = 0;
  private val buffer = new StringBuilder

  def call: String = {
    _write(ast)
    flush()
  }

  def _write(ast: AST) = ast match {
    case IntLit(v) => write(v.toString)
    case DoubleLit(v) => write(v.toString)
    case BoolLit(v) => write(v.toString)
    case ConstLit(v) => write(v.toString)
    case SymbolLit(v) => write(":" + v.toString)
    case StringLit(v) => write(v.toString)
    case Keyword(v) => write(v.toString)
    case LVar(v) => write(v.toString)
    case IVar(v) => write("@" + v.toString)
    case GVar(v) => write("$" + v.toString)
    case Binary(op, lhs, rhs) => write(format(lhs) + s" ${Op.stringfy(op)} " + format(rhs))
    case Assign(lhs, rhs, op) => write(format(lhs) + s" ${Op.stringfy(op)} " + format(rhs))
    case Ary(vars) => write("[" + joinWithComma(vars) + "]")
    case ARef(v, ref) => write(format(v) + "[" + format(ref) + "]")
    case Hash(maps) => {
      val kvs = maps.map { kv => format(kv._1) + " => " + format(kv._2) }.mkString(", ")
      if (maps.size == 0) write("{}") else write("{ " + kvs + " }")
    }
    case IfExpr(cond, stmnts, elsifBody, fBody) => {
      writeln("if " + format(cond))
      writeBody(stmnts)
      fBody match {
        case Some(body) =>
          indentedWrite("else"+"\n")
          writeBody(body)
        case _ => // nothing
      }
      elsifBody.foreach {
        case ElsifBody(cond, body) =>
          indentedWrite("elsif "+ format(cond)+"\n")
          writeBody(body)
      }
      indentedWrite("end")
    }
    case UnlessExpr(cond, stmnts, f_body) => {
      writeln("unless " + format(cond))
      writeBody(stmnts)
      indentedWrite("end")
    }
    case WhileExpr(cond, stmnts) => {
      writeln("while " + format(cond))
      writeBody(stmnts)
      indentedWrite("end")
    }
    case UntilExpr(cond, stmnts) => {
      writeln("until " + format(cond))
      writeBody(stmnts)
      indentedWrite("end")
    }
    case IfModExpr(cond, expr) => write(format(expr) + " if " + format(cond))
    case UnlessModExpr(cond, expr) => write(format(expr) + " unless " + format(cond))
    case Return(args) => {
      val ret = if (args.size == 0) "" else " " + joinWithComma(args)
      write(s"return" + ret)
    }
    case Cmd(rev, name, arg, block) => {
      val recver = rev.fold("")(format(_)+".")
      val args = arg.fold("")(" " + format(_))
      val blck = block.fold("")(format(_))
      write(recver + name + args + blck)
    }
    case Call(rev, name, arg, block) => {
      val recver = rev.fold("")(format(_)+".")
      val args = arg.fold("")("(" + format(_) + ")")
      val blck = block.fold("")(format(_))
      write(recver + name + args + blck)
    }
    case DoBlock(args, stmnts) => {
      writeln(" do" + args.fold("") (" |" + format(_) + "|"))
      writeBody(stmnts)
      indentedWrite("end")
    }
    case BraceBlock(args, stmnts) => {
      if (stmnts.v.size < 2) {
        write(" {" + args.fold("") (" |" + format(_) + "| "))
        stmnts.foreach { stmnt => write(format(stmnt)) }
        write(" }")
      } else {
        writeln(" {" + args.fold("") (" |" + format(_) + "|"))
        writeBody(stmnts)
        indentedWrite("}")
      }
    }
    case Unary(op, expr) => {
      write(Op.stringfy(op))
      write(if (expr.isInstanceOf[Binary]) s"(${format(expr)})" else format(expr))
    }
    case ActualArgs(names) => write(constructArgument(names))
    case FormalArgs(vars) => write(constructArgument(vars))
    case ClassExpr(name, parent, Stmnts(stmnts)) => classNested {
      writeln("class " + format(name) + parent.fold("")(" < "+format(_)) )
      nested {
        for (i <- 0 until stmnts.length) {
          val cr = if (i == stmnts.length-1) "\n" else "\n\n"
          indentedWrite(format(stmnts(i))+cr)
        }
      }
      indentedWrite("end")
    }
    case ModuleExpr(name, Stmnts(stmnts)) => classNested {
      writeln("module " + format(name))
      nested {
        for (i <- 0 until stmnts.length) {
          val cr = if (i == stmnts.length-1) "\n" else "\n\n"
          indentedWrite(format(stmnts(i))+cr)
        }
      }
      indentedWrite("end")
    }
    case DefExpr(name, args, stmnts) => {
      writeln("def "+ name + args.fold("") ("(" + format(_) + ")"))
      writeBody(stmnts)
      indentedWrite("end")
    }
    case Stmnts(stmnts) => {
      val s = stmnts.size
      if (classDepth > 0) {
        for (i <- 0 until s) {
          stmnts(i) match {
            case Operators(_) | Operator(_, _, _) | Syntax(_, _) => // noop
            case stmnt if i == s => writeln(format(stmnt))
            case stmnt@(ClassExpr(_, _, _) | ModuleExpr(_, _)) => writeln(format(stmnt)+"\n")
            case stmnt => write(format(stmnt))
          }
        }
      } else {
        for (i <- 0 until s) {
          stmnts(i) match {
            case Operators(_) | Operator(_, _, _) | Syntax(_, _) => // noop
            case stmnt@(DefExpr(_, _, _) | ClassExpr(_, _, _) | ModuleExpr(_, _)) => writeln(format(stmnt)+"\n")
            case stmnt => writeln(format(stmnt))
          }
        }
      }
    }
    case _ => // noop
  }

  private def flush(): String = buffer.toString()

  private def write(text: String) = buffer.append(text)

  private def writeln(text: String) = write(text+"\n")

  private def indentedWrite(txt: String) = write(indented+txt)

  private def indented: String = "  " * depth

  private def classNested(fn: => Unit) = {
    classDepth += 1
    fn
    classDepth -= 1
  }

  private def nested(fn: => Unit) = {
    depth += 1
    fn
    depth -= 1
  }

  private def writeBody(stmnts: Stmnts) = nested {
    stmnts.foreach { stmnt => indentedWrite(format(stmnt)+"\n") }
  }

  private def format(ast: AST): String = PrettyPrinter(ast, depth).call

  private def joinWithComma(vars: List[Expr]): String = vars.map(format(_)).mkString(", ")

  private def constructArgument(vars: List[ArgElement]): String = vars.map {
    case KeywordArgElement(key, value) => key + ": " + format(value)
    case DefaultArgElement(key, value) => key + " = " + format(value)
    case SimpleArgElement(value) => value
    case ActualArgElement(value) => format(value)
  }.mkString(", ")
}
