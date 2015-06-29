package spike

object Interpreter {
  type Scope = Map[String, Expression]
  val EmptyScope: Scope = Map.empty

  def withVars(s: Scope, vars: Seq[Expression]): Scope =
    vars match {
      case Nil => s
      case AtomExpression(name) :: (value: Expression) :: rest =>
        withVars(s.updated(name, interpret(value, s)._1), rest)
    }

  def wrapList(exp: List[Expression]) =
    exp match {
      case hd :: Nil => hd
      case _ => ListExpression(exp)
    }

  def funCall(fun: FnExpression, args: Seq[Expression], scope: Scope) = {
    val fnScope = args.zip(fun.args)
      .foldLeft(scope) { case (acc, (value, name)) => acc.updated(name, value) }

    interpret(fun.body, fnScope)
  }

  def interpret(ast: Expression, s: Scope = EmptyScope): (Expression, Scope) =
    ast match {
      case AtomExpression(varname) => (s(varname), s)

      case QuoteExpression(exp) => (exp, s)

      case ListExpression(AtomExpression("cond") :: code) =>
        code.collectFirst{case ListExpression(Seq(Std.Truthy(), stmt)) => stmt} match {
          case None => (ListExpression(Nil), s)
          case Some(stmt) => interpret(stmt, s)
        }

      case ListExpression(AtomExpression("let") :: ListExpression(vars) :: code) =>
        val scope = withVars(s, vars)
        val res = code.map(interpret(_, scope))
        res.last

      case ListExpression(AtomExpression("lambda") :: ListExpression(argExps) :: code) =>
        val args = for (AtomExpression(arg) <- argExps) yield arg
        (FnExpression(args, wrapList(code)), s)

      case ListExpression(AtomExpression(fun) :: args) =>
        val (evaluatedArgs, scope) = args.foldLeft((List.empty[Expression], s)) {
          case ((acc, scope), x) =>
            val (exp, newScope) = interpret(x, scope)
            (exp :: acc, newScope)
        }
        fun match {
          case "+" => (Std.add(evaluatedArgs).get, scope)
          case "*" => (Std.mul(evaluatedArgs).get, scope)

          case "print" =>
            for (arg <- evaluatedArgs) {
              print(Printer(arg))
              print(" ")
            }
            println("")
            (ListExpression(Nil), scope)

          case "call" =>
            val (fn: FnExpression) :: fnArgs = evaluatedArgs.reverse
            funCall(fn, fnArgs, scope)

          case _ =>
            scope(fun) match {
              case x: FnExpression => funCall(x, evaluatedArgs, scope)
            }
        }

      case _: LiteralExpression => (ast, s)

      case exp =>
        throw new RuntimeException(exp.toString)
    }

  def apply(ast: Expression)(implicit s: Scope = Map.empty) = {
    val (exp, scope) = interpret(ast, s)
    exp
  }
}
