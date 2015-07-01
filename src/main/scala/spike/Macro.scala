package spike

object Macro {
  type Scope = Map[String, Expression]
  type Scoped[T] = (T, Scope)

  def merge[K, V](a: Map[K, V], b: Map[K, V]): Map[K, V] =
    a ++ b

  implicit class ScopedExt[T](val self: Scoped[T]) extends AnyVal {
    def scope: Scope = self._2
    def apply(): T = self._1

    def flatMap[R](fn: T => Scoped[R]): Scoped[R] = {
      val (res, newScope) = fn(self._1)
      (res, merge(self._2, newScope))
    }

    def map[R](fn: T => R): Scoped[R] = (fn(self._1), self._2)

    def then[R](o: R): Scoped[R] = (o, self._2)
  }
  
  def traverse[T](xs: Scoped[List[T]])(fn: Scoped[T] => Scoped[T]): Scoped[List[T]] = {
    // @scala.annotation.tailrec
    def rec(xs: Scoped[List[T]], acc: List[T]): Scoped[List[T]] =
      xs flatMap {
        case Nil => xs then acc.reverse
        case hd :: tl =>
          val res = fn(xs then hd)
          rec(res then tl, res() :: acc)
      }
    rec(xs, Nil)
  }

  def unquote(ast: Scoped[Expression]): Expression =
    ast._1 match {
      case UnquoteExpression(name) =>
        ast.scope(name)
      case ListExpression(xs) =>
        ListExpression(xs map {x => unquote(ast then x)})
      case exp => exp
    }

  def run(ast: Scoped[Expression]): Scoped[Expression] =
    ast flatMap {
      case ListExpression(AtomExpression("defmacro") :: AtomExpression(name) :: ListExpression(args) :: body :: Nil) =>
        val theArgs = for (AtomExpression(arg) <- args) yield arg
        (ListExpression(Nil), ast.scope.updated(name, FnExpression(theArgs, body)))

      case exp @ ListExpression(AtomExpression(name) :: args) =>
        ast.scope.get(name) match {
          case Some(fn @ FnExpression(_, _)) =>
            val code = Interpreter.funCall(fn, args, ast.scope)
            Macro.run(ast then unquote(code))

          case _ =>
            val res = traverse(ast then args)(run _)
            traverse(ast then args)(run _) map {x => ListExpression(AtomExpression(name) :: x)}
        }

      case _ => ast
    }

  def apply(ast: Expression) = run((ast, Map.empty))._1
}
