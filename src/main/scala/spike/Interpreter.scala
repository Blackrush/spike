package spike

object Interpreter {
  def apply(ast: Expression): Expression =
    ast match {
      case ListExpression(AtomExpression(fun) :: args) =>
        val evaluatedArgs = for (arg <- args) yield Interpreter(arg)
        fun match {
          case "+" => Std.add(evaluatedArgs).get
          case "*" => Std.mul(evaluatedArgs).get

          case "print" =>
            for (arg <- evaluatedArgs) {
              print(Printer(arg))
              print(" ")
            }
            println("")
            ListExpression(Nil)
        }
      case _: LiteralExpression => ast
      case exp =>
        throw new RuntimeException(exp.toString)
    }
}
