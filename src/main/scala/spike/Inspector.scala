package spike

object Inspector {
  def inspect(ast: Expression): String =
    ast match {
      case IntExpression(x) => x.toString
      case RealExpression(x) => x.toString
      case StrExpression(x) => "\"" + x + "\""
      case AtomExpression(x) => x
      case ListExpression(list) => list.map(inspect _).mkString("(", " ", ")")
      case QuoteExpression(exp) => "'" + inspect(exp)
    }

  def apply(ast: Expression) = inspect(ast)
}
