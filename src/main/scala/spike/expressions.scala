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

sealed trait UnaryExpression extends Expression

case class QuoteExpression(exp: Expression) extends UnaryExpression

case class FnExpression(args: Seq[String], body: Expression) extends Expression
