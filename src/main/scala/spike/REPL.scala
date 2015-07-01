package spike

object REPL {
  def main(args: Array[String]): Unit = {
    def run(scope: Map[String, Expression]): Unit = {
      print("> ")
      val scanner = new java.util.Scanner(java.lang.System.in)
      val line = scanner.nextLine()

      val ast = Parser(line)
      val (newAst, astScope) = Macro.run((ast, scope))
      val (res, newScope) = Interpreter.interpret(newAst, astScope)
      println(Inspector(res))

      run(newScope)
    }

    run(Map.empty)
  }
}

