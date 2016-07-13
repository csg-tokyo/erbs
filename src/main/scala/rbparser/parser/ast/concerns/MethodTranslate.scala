package rbparser.parser.ast
package concerns

trait MethodTranslate {
  val MODULE_NAME = "Operator"

  val syntaxBody: List[String]
  val syntaxTags: Map[String, Expr]
  val tags: Set[String]
  val body: Expr

  def toMethodCall(map: Map[String, Expr]): Call = Call(None, callingName, toCallingArgs(map), None)

  def toMethod: DefExpr = DefExpr(s"self.${methodName}", formalArgs, Stmnts(List(body)))

  lazy val className: String = tags.map(_.capitalize).toList.sorted.reduceLeft(_ + _)

  private lazy val baseName = s"${MODULE_NAME}::${className}"

  private lazy val callingName = s"${baseName}::${methodName}"

  private lazy val methodName = "op_" + normalizeSymbol(
    syntaxBody.map {
      x => syntaxTags.get(x).fold(x)(_ => x.capitalize)
    }.mkString("_")
  )

  private lazy val formalArgs = syntaxTags.keys.toList.map(LVar(_)) match {
    case Nil => None
    case x => Some(FormalArgs(x))
  }

  private def toCallingArgs(map: Map[String, Expr]) = syntaxTags.keys.toList.map { map.get(_).get } match {
    case Nil => None
    case x => Some(ActualArgs(x))
  }

  private def normalizeSymbol(str: String): String = str.map {
    case x if (x >= '0' && x <= '9') || (x >= 'a' && x <= 'z') || (x >= 'A' &&  x <= 'Z') || x == '_' || x == '_' => x
    case x =>  x.toInt.toString
  }.mkString("")
}
