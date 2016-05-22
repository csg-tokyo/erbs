package rbparser

object PrettyPrinter {
  def call(p: ASTs): String = p match {
    case literal: Literal[_] => literal.toString()
    case Ary(None) => "[]"
    case Ary(Some(lst)) => "[" + lst.map(x => call(x)).mkString(", ") + "]"
    case Binary(op, lhs, rhs) => s"""${call(lhs)} ${Op.stringfy(op)} ${call(rhs)}"""
    case Assign(lhs, rhs) => s"""${call(lhs)} = ${call(rhs)}"""
    case Call(rev, name, arg, block) =>  s"""${rev.fold("")(x => call(x)+".")}$name${arg.fold("")(call(_))}${block.fold("")(call(_))}"""
    case Block(args, body) => s""" ${args.fold("") (e => "|" + call(e) + "|")}${call(body)}"""
    case Stmnts(v) => v.map(call(_)).mkString("\n")
    case ARef(v, ref) => s"""${call(v)}[${call(ref)}]"""
  }
}
