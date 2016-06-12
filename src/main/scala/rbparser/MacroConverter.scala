package rbparser

object MacroConverter {
  def convert(body: Expr, m: Map[String, Expr]): Expr = body match {
    case v@LVar(k) => m.getOrElse(k, v)
    case ARef(recv, ref) => ARef(convert(recv, m), convert(ref, m))
    case Ary(v) => Ary(v.map(convert(_, m)))
    case Hash(hsh) => {
      val mb = Map.newBuilder[Expr, Expr]
      for ((k, v) <- hsh) { mb += convert(k, m) -> convert(v, m) }
      Hash(mb.result)
    }
    case IfExpr(cond, body) => IfExpr(convert(cond, m), body.map(convert(_, m)))
    case IfModExpr(cond, expr) => IfModExpr(convert(cond, m), convert(expr, m))
    case UnlessExpr(cond, body) => UnlessExpr(convert(cond, m), body.map(convert(_, m)))
    case UnlessModExpr(cond, expr) => UnlessModExpr(convert(cond, m), convert(expr, m))
    case Return(args) => Return(args.map(convert(_, m)))
    case Unary(op, v) => Unary(op, convert(v, m))
    case Binary(op, lhs, rhs) => Binary(op, convert(lhs, m), convert(rhs, m))
    case Call(rev, name, args, block) => {
      val as = args.map { case ActualArgs(x) => ActualArgs(x.map(convert(_, m))) }
      val b = block.map(convert(_, m).asInstanceOf[Block]) // fix?
      Call(rev.map(convert(_, m)), name, as, b)
    }
    case Cmd(rev, name, args, block) => {
      val as = args.map { case ActualArgs(x) => ActualArgs(x.map(convert(_, m))) }
      val b = block.map(convert(_, m).asInstanceOf[Block]) // fix?
      Cmd(rev.map(convert(_, m)), name, as, b)
    }
    case Assign(target, value, op) => Assign(convert(target, m), convert(value, m), op)
    case ClassExpr(name, body) => ClassExpr(name, body.map(convert(_, m)))
    case ModuleExpr(name, body) => ModuleExpr(name, body.map(convert(_, m)))
    case DefExpr(name, args, body) => {
      val LVar(n) = m.getOrElse(name, LVar(name)) // FIX?
      DefExpr(n, args, body.map(convert(_, m)))
    }
    case DoBlock(args, body) => DoBlock(args, body.map(convert(_, m)))
    case BraceBlock(args, body) => BraceBlock(args, body.map(convert(_, m)))
    case x => x
  }
}
