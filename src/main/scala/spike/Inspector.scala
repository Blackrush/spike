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
      case UnquoteExpression(name) => "#" + name
      case FnExpression(args, body) => "(lambda " + args.mkString("(", " ", ")") + inspect(body) + ")"
    }

  def apply(ast: Expression) = inspect(ast)
}
