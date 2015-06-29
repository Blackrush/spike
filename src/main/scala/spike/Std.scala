package spike

object Std {
  def foldl(xs: List[Expression], acc: Option[Expression] = None)(fn: (Expression, Expression) => Expression): Option[Expression] =
    xs match {
      case Nil => acc
      case hd :: tl => foldl(tl, acc.map(fn(_, hd)).orElse(Some(hd)))(fn)
    }

  def add(xs: List[Expression]) = foldl(xs) {
    case (IntExpression(a), IntExpression(b)) => IntExpression(a + b)
    case (a: NumExpression, b: NumExpression) => RealExpression(a.real + b.real)
  }

  def sub(xs: List[Expression]) = foldl(xs) {
    case (IntExpression(a), IntExpression(b)) => IntExpression(a - b)
    case (a: NumExpression, b: NumExpression) => RealExpression(a.real - b.real)
  }

  def mul(xs: List[Expression]) = foldl(xs) {
    case (IntExpression(a), IntExpression(b)) => IntExpression(a * b)
    case (a: NumExpression, b: NumExpression) => RealExpression(a.real * b.real)
  }

  def div(xs: List[Expression]) =
    xs match {
      case List(num: NumExpression, div: NumExpression) =>
        val res = num.real / div.real
        if (res.toInt == res)
          IntExpression(res.toInt)
        else
          RealExpression(res)
    }

  def truthy(exp: Expression) =
    exp match {
      case AtomExpression("nil") => false
      case IntExpression(0) => false
      case RealExpression(0.0) => false
      case StrExpression("") => false
      case ListExpression(Nil) => false
      case _ => true
    }

  object Truthy {
    def unapply(exp: Expression): Boolean = truthy(exp)
  }
}
