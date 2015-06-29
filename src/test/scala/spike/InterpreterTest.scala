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

    describe("lambda functions") {
      it("should be constructed") {
        val FnExpression(args, body) = Interpreter(Parser("(lambda (a b c) (+ a b c))"))
        assert(args == List("a", "b", "c"))
        assert(body == ListExpression(Seq(AtomExpression("+"), AtomExpression("a"), AtomExpression("b"), AtomExpression("c"))))
      }

      it("should be executed") {
        val IntExpression(10) = Interpreter(Parser("(let (plus (lambda (a b) (+ a b))) (plus 5 5))"))
      }

      it("should be called") {
        val IntExpression(10) = Interpreter(Parser("(let (plus (lambda (a b) (+ a b))) (call plus 5 5))"))
      }
    }
  }
}

