package g4

class CompilerSpec extends org.scalatest.freespec.AnyFreeSpec:
  val settings1: Settings = Settings(3, 5, 5)
  "The compiler should process single commands" - {

    "COPY #456 TO 3" in {
      val codeLines = Vector("COPY #456 TO 3")
      val code = Compiler(settings1, codeLines).code
      assert(code == Vector(Vector("COPY #4 TO 3", "COPY #5 TO 3", "COPY #6 TO 3")))
    }

    "COPY 0 TO 1" in {
      val codeLines = Vector("COPY 0 TO 1")
      val code = Compiler(settings1, codeLines).code
      assert(code == Vector(Vector("COPY 0 TO 1", "COPY 0 TO 1", "COPY 0 TO 1")))
    }

    "COPY #x TO 4" in {
      val codeLines = Vector("COPY #x TO 4")
      val code = Compiler(settings1, codeLines).code
      assert(code == Vector(Vector("COPY #x TO 4", "COPY #x TO 4", "COPY #x TO 4")))
    }
  }