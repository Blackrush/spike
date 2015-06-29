package spike

object Parser {
  @scala.annotation.tailrec
  def ignoreWhitespaces(input: String, from: Int): Int =
    if (from < input.length && Character.isWhitespace(input(from)))
      ignoreWhitespaces(input, from+1)
    else
      from

  def parseList(input: String, from: Int, acc: List[Expression] = Nil): (ListExpression, Int) =
    input(from) match {
      case ')' => (ListExpression(acc.reverse), from+1)

      case _ =>
        var (exp, end) = subparse(input, from)
        parseList(input, ignoreWhitespaces(input, end), exp :: acc)
    }

  def parseStr(input: String, from: Int, acc: String = "", escape: Boolean = false): (StrExpression, Int) = {
    input(from) match {
      case '"' if !escape => (StrExpression(acc.toString), from+1)
      case '\\' if !escape => parseStr(input, from+1, acc, true)
      case _ => parseStr(input, from+1, acc + input(from), false)
    }
  }

  def parseNumber(input: String, from: Int, acc: String = "", real: Boolean = false): (NumExpression, Int) =
    if (from >= input.length)
      if (real)
        (RealExpression(acc.toDouble), from)
      else
        (IntExpression(acc.toInt), from)
    else
      input(from) match {
        case '.' if !real => parseNumber(input, from+1, acc + input(from), true)
        case x if x=='-' || x>='0' && x<='9' => parseNumber(input, from+1, acc + input(from), real)
        case _ =>
          if (real)
            (RealExpression(acc.toDouble), from)
          else
            (IntExpression(acc.toInt), from)
      }

  @scala.annotation.tailrec
  def parseAtom(input: String, from: Int, acc: String = ""): (AtomExpression, Int) =
    if (from >= input.length || Character.isWhitespace(input(from)) || input(from) == ')')
      (AtomExpression(acc), from)
    else
      parseAtom(input, from+1, acc + input(from))

  def subparse(input: String, from: Int): (Expression, Int) =
    input(from) match {
      case ''' =>
        val (exp, end) = subparse(input, from+1)
        (QuoteExpression(exp), end)
      case '(' => parseList(input, from+1)
      case '"' => parseStr(input, from+1)
      case '-' => parseNumber(input, from)
      case x if x>='0' && x<='9' => parseNumber(input, from)
      case _ => parseAtom(input, from)
    }

  def parse(input: String) = {
    val (exp, _) = subparse(input, 0)
    exp
  }

  def apply(input: String) = parse(input)
}
