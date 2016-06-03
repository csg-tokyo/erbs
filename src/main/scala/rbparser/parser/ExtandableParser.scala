package rbparser
package parser

class ExtendableParser extends RubyParser with OperatorToken {
  override def reserved = K_OPERATOR | super.reserved
  override def primary  = defop | super.primary

  // Parse each item of syntax
  protected lazy val v = """[^}\s]+""".r ^^ LVar

  // TODO fix, ary and hash are tmporary
  protected lazy val opTagPredicate: PackratParser[Hash] = T_WHERE ~> hash
  protected lazy val opSyntax: PackratParser[Syntax] = ("{" ~> v.+ <~ "}") ~ opTagPredicate ^^ {
    case list ~ tagp => Syntax(tagp, Ary(list))
  }
  protected lazy val opSemantics: PackratParser[Expr] = "{" ~> stmnt <~ "}"
  protected lazy val opTags: PackratParser[FormalArgs] = formalArgs

  private lazy val defop: PackratParser[Operator] = (T_OPERATOR ~> opTags) ~ opSyntax ~ ("=>" ~> opSemantics <~ "end") ^^ {
    case tags ~ syntax ~ body => Operator(tags, syntax, body)
  }

}

trait OperatorToken {
  val T_OPERATOR = "operator_with"
  val K_OPERATOR =  """operator_with\b""".r

  val T_WHERE = "where"
  val K_WHERE =  """where\b""".r
}
