package spike

sealed trait Expression

sealed trait LiteralExpression extends Expression

sealed trait NumExpression extends LiteralExpression {
  def real: Double
}
case class IntExpression(i: Int) extends NumExpression {
  override def real = i
}
case class RealExpression(r: Double) extends NumExpression {
  override def real = r
}
case class StrExpression(s: String) extends LiteralExpression
case class AtomExpression(a: String) extends LiteralExpression

case class ListExpression(l: Seq[Expression]) extends Expression
object ListExpression {
  def lift(exp: List[Expression]) =
    exp match {
      case hd :: Nil => hd
      case _ => ListExpression(exp)
    }
}

sealed trait UnaryExpression extends Expression

case class QuoteExpression(exp: Expression) extends UnaryExpression
case class UnquoteExpression(name: String) extends Expression

case class FnExpression(args: Seq[String], body: Expression) extends Expression
