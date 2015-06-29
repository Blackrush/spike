package spike

object Interpreter {
  type Scope = Map[String, Expression]
  val EmptyScope: Scope = Map.empty

  def withVars(s: Scope, vars: Seq[Expression]): Scope =
    vars match {
      case Nil => s
      case AtomExpression(name) :: (value: Expression) :: rest =>
        withVars(s.updated(name, value), rest)
    }

  def apply(ast: Expression)(implicit s: Scope = EmptyScope): Expression =
    ast match {
      case AtomExpression(varname) => s(varname)

      case QuoteExpression(exp) => exp

      case ListExpression(AtomExpression("cond") :: code) =>
        code.collectFirst{case ListExpression(Seq(Std.Truthy(), stmt)) => stmt} match {
          case None => ListExpression(Nil)
          case Some(stmt) => Interpreter(stmt)
        }

      case ListExpression(AtomExpression("let") :: ListExpression(vars) :: code) =>
        val scope = withVars(s, vars)
        val res = for (stmt <- code) yield Interpreter(stmt)(scope)
        res.last

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
