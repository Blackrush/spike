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

  @scala.annotation.tailrec
  def executeExpressions(scope: Scope, xs: List[Expression]): (Expression, Scope) =
    xs match {
      case Nil => (ListExpression(Nil), scope)
      case hd :: Nil => interpret(hd, scope)
      case hd :: tl =>
        val (res, newScope) = interpret(hd, scope)
        executeExpressions(newScope, tl)
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
        executeExpressions(withVars(s, vars), code)

      case ListExpression(AtomExpression("lambda") :: ListExpression(argExps) :: code) =>
        val args = for (AtomExpression(arg) <- argExps) yield arg
        (FnExpression(args, wrapList(code)), s)

      case ListExpression(AtomExpression("do") :: code) =>
        executeExpressions(s, code)

      case ListExpression(AtomExpression(fun) :: args) =>
        val evaluatedArgs = args.map(x => interpret(x, s)._1)
        fun match {
          case "+" => (Std.add(evaluatedArgs).get, s)
          case "-" => (Std.sub(evaluatedArgs).get, s)
          case "*" => (Std.mul(evaluatedArgs).get, s)
          case "/" => (Std.div(evaluatedArgs), s)
          case ">" => (Std.gt(evaluatedArgs), s)
          case "<" => (Std.lt(evaluatedArgs), s)
          case "!" => (Std.not(evaluatedArgs), s)
          case "=" => (Std.eq(evaluatedArgs).get, s)
          case "&" => (Std.and(evaluatedArgs), s)
          case "|" => (Std.or(evaluatedArgs), s)
          case "cons" => (Std.cons(evaluatedArgs), s)
          case "hd" => (Std.hd(evaluatedArgs), s)
          case "tl" => (Std.tl(evaluatedArgs), s)

          case "print" =>
            for (arg <- evaluatedArgs) {
              print(Printer(arg))
              print(" ")
            }
            println("")
            (ListExpression(Nil), s)

          case "call" =>
            val (fn: FnExpression) :: fnArgs = evaluatedArgs
            funCall(fn, fnArgs, s)

          case _ =>
            s.get(fun) match {
              case None => throw new RuntimeException(s"the function ${fun} does not exist")
              case Some(x: FnExpression) => funCall(x, evaluatedArgs, s)
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
