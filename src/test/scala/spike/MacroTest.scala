package spike

import org.scalatest.FunSpec
import org.scalatest.Assertions

class MacroTest extends FunSpec {
  import Assertions._

  describe("The Spike Macro System") {
    describe("a macro") {
      it("should be suppressed") {
        val ListExpression(Nil) = Macro(Parser("(defmacro hello () (print \"hello\"))"))
      }
      it("should be invoked") {
        val ast = Macro(Parser("(do (defmacro hello (x) '(print \"hello\" #x)) (hello \"monde\"))"))
        assert(ast == ListExpression(List(AtomExpression("do"), ListExpression(Nil), ListExpression(List(
          AtomExpression("print"), StrExpression("hello"), StrExpression("monde"))))))
      }
    }
    it("if macro") {
      val AtomExpression("bar") = Interpreter(Macro(Parser("""
(defmacro if (exp do else)
  '(cond (#exp #do) ('else #else)))

(defmacro unless (exp do else)
  '(if (! #exp) #do #else))

(defmacro iff (exp do else)
  '(unless (! #exp) #do #else))

(let (name (if (> 1 2) "world" "monde"))
     (print "hello" name))

(if (> 1 2) 'foo 'bar)

(unless 'nil 'bar 'foo)

(iff 'nil 'foo 'bar)
""")))
    }
  }
}

