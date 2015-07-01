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

  def wrapList(exp: List[Expression]) = ListExpression.lift(exp)

  def funCall(fun: FnExpression, args: Seq[Expression], scope: Scope) = {
    val fnScope = args.zip(fun.args)
      .foldLeft(scope) { case (acc, (value, name)) => acc.updated(name, value) }

    interpret(fun.body, fnScope)
  }

  def interpret(ast: Expression, s: Scope = EmptyScope): (Expression, Scope) =
    ast match {
      case ListExpression(Nil) => (ast, s)

      case AtomExpression(varname) => (s(varname), s)

      case QuoteExpression(exp) => (exp, s)

      case ListExpression(AtomExpression("def") :: AtomExpression(name) :: code :: Nil) =>
        val res = interpret(code)._1
        (ListExpression(Nil), s.updated(name, res))

      case ListExpression(AtomExpression("cond") :: code) =>
        code.dropWhile { case ListExpression(Seq(cond, _)) => !Std.truthy(interpret(cond, s)._1) } match {
          case ListExpression(Seq(_, exp)) :: tl => interpret(exp, s)
        }

      case ListExpression(AtomExpression("let") :: ListExpression(vars) :: code) =>
        val scope = withVars(s, vars)
        val res = code.map(interpret(_, scope))
        res.last

      case ListExpression(AtomExpression("lambda") :: ListExpression(argExps) :: code) =>
        val args = for (AtomExpression(arg) <- argExps) yield arg
        (FnExpression(args, wrapList(code)), s)

      case ListExpression(AtomExpression(fun) :: args) =>
        val (tmpEvaluatedArgs, scope) = args.foldLeft((List.empty[Expression], s)) {
          case ((acc, scope), x) =>
            val (exp, newScope) = interpret(x, scope)
            (exp :: acc, newScope)
        }
        val evaluatedArgs = tmpEvaluatedArgs.reverse
        fun match {
          case "+" => (Std.add(evaluatedArgs).get, scope)
          case "-" => (Std.sub(evaluatedArgs).get, scope)
          case "*" => (Std.mul(evaluatedArgs).get, scope)
          case "/" => (Std.div(evaluatedArgs), scope)
          case ">" => (Std.gt(evaluatedArgs), scope)
          case "<" => (Std.lt(evaluatedArgs), scope)
          case "!" => (Std.not(evaluatedArgs), scope)

          case "print" =>
            for (arg <- evaluatedArgs) {
              print(Printer(arg))
              print(" ")
            }
            println("")
            (ListExpression(Nil), scope)

          case "call" =>
            val (fn: FnExpression) :: fnArgs = evaluatedArgs
            funCall(fn, fnArgs, scope)

          case "do" =>
            (evaluatedArgs.last, scope)

          case _ =>
            scope.get(fun) match {
              case None => throw new RuntimeException(s"the function ${fun} does not exist")
              case Some(x: FnExpression) => funCall(x, evaluatedArgs, scope)
              case Some(x) => throw new RuntimeException(s"${Inspector(x)} is not callable")
            }
        }

      case _: LiteralExpression => (ast, s)

      case exp =>
        throw new RuntimeException(Inspector(exp))
    }

  def apply(ast: Expression)(implicit s: Scope = Map.empty) = {
    val (exp, scope) = interpret(ast, s)
    exp
  }
}
