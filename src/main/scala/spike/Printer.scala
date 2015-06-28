package spike

object Printer {
  def print(ast: Expression): String =
    ast match {
      case IntExpression(x) => x.toString
      case RealExpression(x) => x.toString
      case StrExpression(x) => x
      case ListExpression(list) => list.map(print _).mkString("(", " ", ")")
    }

  def apply(ast: Expression) = print(ast)

}
