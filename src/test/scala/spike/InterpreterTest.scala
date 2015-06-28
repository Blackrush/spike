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
  }
}

