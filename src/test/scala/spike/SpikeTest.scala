package spike

import org.scalatest.FunSpec
import org.scalatest.Assertions

class SpikeTest extends FunSpec {
  import Assertions._

  describe("The Spike Programming Language") {
    it("should be possible to define a fibonacci function") {
      val code = """
(defmacro defn (name args body) '(def #name (lambda #args #body)))

(defn fib (n)
      (cond ((= n 0) 0)
            ((= n 1) 1)
            ('else   (+ (fib (- n 2)) (fib (- n 1))))))

(fib 10)
"""
      val IntExpression(n) = Interpreter(Macro(Parser(code)))
      assert(n == 55)
    }

    it("should be possible to define a function mapping elements of a list") {
      val code = """
(defmacro defn (name args body) '(def #name (lambda #args #body)))
(defmacro if (cond do else) '(cond (#cond #do) ('else #else)))

(defn map (list fn)
      (if list (cons (fn (hd list))
                     (map (tl list)))
               '()))

(defn mul-2 (x)
      (* x 2))

(map '(1 2 3) mul-2)
"""
      val res = Interpreter(Macro(Parser(code)))
      assert(res == ListExpression(List(IntExpression(2), IntExpression(4), IntExpression(6))))
    }
  }
}

