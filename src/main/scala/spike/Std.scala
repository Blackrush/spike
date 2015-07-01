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

      case _ => throw new RuntimeException("invalid arity")
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

      case _ => throw new RuntimeException("invalid arity")
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

      case _ => throw new RuntimeException("invalid arity")
    }

  def eq(xs: List[Expression]) = foldl(xs) {
    case (left: NumExpression, right: NumExpression) => AtomExpression(if (left.real == right.real) "true" else "nil")
    case (left: StrExpression, right: StrExpression) => AtomExpression(if (left.s.equals(right.s)) "true" else "nil")
    case (left: AtomExpression, right: AtomExpression) => AtomExpression(if (left.a.equals(right.a)) "true" else "nil")
    // lists?
  }

  def not(xs: List[Expression]) =
    xs match {
      case hd :: Nil =>
        val b = truthy(hd)
        if (b) AtomExpression("nil") else AtomExpression("true")

      case _ => throw new RuntimeException("invalid arity")
    }

  def and(xs: List[Expression]): Expression =
    xs match {
      case Nil => AtomExpression("true")
      case hd :: tl =>
        if (!truthy(hd))
          AtomExpression("nil")
        else
          and(tl)
    }

  def or(xs: List[Expression]): Expression =
    xs match {
      case Nil => AtomExpression("true")
      case hd :: tl =>
        if (truthy(hd))
          AtomExpression("true")
        else
          or(tl)
    }

  def cons(xs: List[Expression]): Expression =
    xs match {
      case hd :: ListExpression(tl: List[Expression]) :: Nil => ListExpression(hd :: tl)
    }

  def hd(xs: List[Expression]): Expression =
    xs match {
      case ListExpression(hd :: _) :: Nil => hd
    }

  def tl(xs: List[Expression]): Expression =
    xs match {
      case ListExpression(_ :: tl) :: Nil => ListExpression(tl)
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
