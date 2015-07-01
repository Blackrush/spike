package spike

object Printer {
  def print(ast: Expression): String =
    ast match {
      case IntExpression(x) => x.toString
      case RealExpression(x) => x.toString
      case StrExpression(x) => x
      case AtomExpression(x) => "'" + x
      case ListExpression(list) => list.map(print _).mkString("(", " ", ")")
      case FnExpression(_, _) => "[lambda]"
      case _ => "[not printable]"
    }

  def apply(ast: Expression) = print(ast)

}
