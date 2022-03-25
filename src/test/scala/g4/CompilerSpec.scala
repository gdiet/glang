package g4

class CompilerSpec extends org.scalatest.freespec.AnyFreeSpec:
  val settings1: Settings = Settings(3, 5, 5)
  "The compiler should process single commands" - {

    // 1: One actor, least significant first. -1: DIGITS-1 actors.
    "loop: <1> SET F TRUE; <-1> NOP" in {
      val codeLines = Vector("loop: <1> SET F TRUE; <-1> NOP")
      val code = Compiler(settings1, codeLines).code
      assert(code == Vector(Vector("loop: SET F TRUE", "loop: NOP", "loop: NOP")))
    }

    "loop: SET F TRUE" in {
      val codeLines = Vector("loop: SET F TRUE")
      val code = Compiler(settings1, codeLines).code
      assert(code == Vector(Vector("loop: SET F TRUE", "loop: NOP", "loop: NOP")))
    }

    "SET F TRUE" in {
      val codeLines = Vector("SET F TRUE")
      val code = Compiler(settings1, codeLines).code
      assert(code == Vector(Vector("SET F TRUE", "NOP", "NOP")))
    }

    "COPY #456 TO 3" in {
      val codeLines = Vector("COPY #456 TO 3")
      val code = Compiler(settings1, codeLines).code
      assert(code == Vector(Vector("COPY #456 TO 3", "COPY #456 TO 3", "COPY #456 TO 3")))
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

    "COPY and ADD" in {
      val codeLines = Vector("COPY #456 TO 0", "ADD #456 TO 0 OVERFLOW 1")
      val code = Compiler(settings1, codeLines).code
      println(s"### $code")
      assert(code == Vector(
        Vector("COPY #456 TO 0", "COPY #456 TO 0", "COPY #456 TO 0"),
        Vector("ADD #456 TO 0 OVERFLOW 1", "ADD #456 TO 0 OVERFLOW 1", "ADD #456 TO 0 OVERFLOW 1"),
      ))
    }
  }
