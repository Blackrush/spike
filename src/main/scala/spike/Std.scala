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

  def gt(xs: List[Expression]) =
    xs match {
      case List(IntExpression(a), IntExpression(b)) =>
        if (a > b)
          AtomExpression("true")
        else
          AtomExpression("nil")

      case List(RealExpression(a), RealExpression(b)) =>
        if (a > b)
          AtomExpression("true")
        else
          AtomExpression("nil")
    }

  def lt(xs: List[Expression]) =
    xs match {
      case List(IntExpression(a), IntExpression(b)) =>
        if (a < b)
          AtomExpression("true")
        else
          AtomExpression("nil")

      case List(RealExpression(a), RealExpression(b)) =>
        if (a < b)
          AtomExpression("true")
        else
          AtomExpression("nil")
    }

  def not(xs: List[Expression]) =
    xs match {
      case hd :: Nil =>
        val b = truthy(hd)
        if (b) AtomExpression("nil") else AtomExpression("true")
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
