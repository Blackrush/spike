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
  }
}

