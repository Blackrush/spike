package spike

import org.scalatest.FunSpec
import org.scalatest.Assertions

class ParserTest extends FunSpec {
  import Assertions._
  import Parser.parse

  describe("The Spike Parser") {
    describe("must parse literals") {
      it("should accept integers") {
        val IntExpression(0) = parse("0")
        val IntExpression(1) = parse("1")
        val IntExpression(2) = parse("2")
        val IntExpression(3) = parse("3")
        val IntExpression(10) = parse("10")
        val IntExpression(20) = parse("20")
        val IntExpression(999999) = parse("999999")
      }

      it("should accept real numbers") {
        val RealExpression(0.0) = parse("0.0")
        val RealExpression(1.23) = parse("1.23")
      }

      it("should accept strings") {
        val StrExpression(str) = parse("\"hello \\\" world\"")
        assert(str == "hello \" world")
      }

      it("should accept atoms") {
        val AtomExpression("hello") = parse("hello")
      }

      it("should accept lists") {
        val ListExpression(list) = parse("(1 2)")
        assert(list == List(IntExpression(1), IntExpression(2)))

        val ListExpression(list2) = parse("(print \"hello\")")
        assert(list2 == List(AtomExpression("print"), StrExpression("hello")))
      }

      it("should accept nested lists") {
        val ast = parse("(1 2 (3 4 (5 6)))")
        assert(ast == ListExpression(List(
          IntExpression(1), IntExpression(2), ListExpression(List(
            IntExpression(3), IntExpression(4), ListExpression(List(
              IntExpression(5), IntExpression(6))))))))
      }
    }

    it("should accept quoted expressions") {
      val QuoteExpression(ListExpression(list)) = parse("'(hello world)")
      assert(list == List(AtomExpression("hello"), AtomExpression("world")))
    }
  }
}
