package spike

import org.scalatest.FunSpec
import org.scalatest.Assertions

class InterpreterTest extends FunSpec {
  import Assertions._
  
  describe("The Spike Interpreter") {
    it("should perform simple arithmetics") {
      val ast = Parser("(+ 1 2 (* 3 5))")
      val IntExpression(res) = Interpreter(ast)
      assert(res == 18)
    }

    it("should print messages") {
      Interpreter(Parser("(print \"Hello, World!\")"))
    }

    it("should store and get variables") {
      val IntExpression(res) = Interpreter(Parser("(let (a 1 b 2) (+ a b))"))
      assert(res == 3)
    }

    it("should perform conditional branching") {
      val ast = Parser("(cond ((> a 0) 'foo) (< a -10) 'bar) (true 'buz))")
      val AtomExpression("foo") = Interpreter(ast)(Map("a" -> IntExpression(20)))
      val AtomExpression("foo") = Interpreter(ast)(Map("a" -> IntExpression(-20)))
      val AtomExpression("foo") = Interpreter(ast)(Map("a" -> IntExpression(-5)))
    }
  }
}

