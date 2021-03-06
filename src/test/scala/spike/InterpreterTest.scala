package spike

import org.scalatest.FunSpec
import org.scalatest.Assertions

class InterpreterTest extends FunSpec {
  import Assertions._
  
  describe("The Spike Interpreter") {
    it("should perform simple arithmetics") {
      val IntExpression(18) = Interpreter(Parser("(+ 1 2 (* 3 (- 10 (/ 10 2))))"))
    }

    it("should perform simple list operations") {
      val IntExpression(1) = Interpreter(Parser("(hd '(1 2 3))"))
      val ListExpression(IntExpression(2) :: Nil) = Interpreter(Parser("(tl '(1 2))"))
      val ListExpression(IntExpression(1) :: Nil) = Interpreter(Parser("(cons 1 '())"))
    }

    it("should print messages") {
      Interpreter(Parser("(print \"Hello, World!\")"))
    }

    it("should store and get variables") {
      val IntExpression(res) = Interpreter(Parser("(let (a 1 b 2) (+ a b))"))
      assert(res == 3)
    }

    it("should perform conditional branching") {
      val ast = Parser("(cond ((> a 0) 'foo) ((< a -10) 'bar) ('else 'buz))")
      val AtomExpression("foo") = Interpreter(ast)(Map("a" -> IntExpression(20)))
      val AtomExpression("bar") = Interpreter(ast)(Map("a" -> IntExpression(-20)))
      val AtomExpression("buz") = Interpreter(ast)(Map("a" -> IntExpression(-5)))
    }

    it("should execute sequentially a block") {
      val IntExpression(10) = Interpreter(Parser("(do (print \"hello\") (+ 5 5))"))
    }

    describe("lambda functions") {
      it("should be constructed") {
        val FnExpression(args, body) = Interpreter(Parser("(lambda (a b c) (+ a b c))"))
        assert(args == List("a", "b", "c"))
        assert(body == ListExpression(Seq(AtomExpression("+"), AtomExpression("a"), AtomExpression("b"), AtomExpression("c"))))
      }

      it("should be executed") {
        val IntExpression(10) = Interpreter(Parser("(let (plus (lambda (a b) (+ a b))) (plus 5 5))"))
        val IntExpression(-3) = Interpreter(Parser("(let (minus (lambda (a b c) (- a b c))) (minus 0 1 2))"))
      }

      it("should be called") {
        val IntExpression(10) = Interpreter(Parser("(let (plus (lambda (a b) (+ a b))) (call plus 5 5))"))
      }
    }

    it("should negate expressions") {
      val AtomExpression("true") = Interpreter(Parser("(! 'nil)"))
      val AtomExpression("nil") =  Interpreter(Parser("(! 'true)"))
    }

    it("should be possible to define variables") {
      val (ListExpression(Nil), scope) = Interpreter.interpret(Parser("(def hello \"world\")"))
      assert(scope == Map("hello" -> StrExpression("world")))
    }
  }
}

