package g4

class CodeExamplesSpec extends org.scalatest.freespec.AnyFreeSpec:
  val settings1: Settings = Settings(3, 5, 5)
  "The compiler should process single commands" - {

    "COPY #456 TO 3" in {
      val codeLines = Vector("COPY #456 TO 3")
      val code = Compiler(settings1, codeLines).code
      assert(code == Vector("COPY #4 TO 3", "COPY #5 TO 3", "COPY #6 TO 3"))
    }

    "COPY 0 TO 1" in {
//      val codeLines = Vector(Vector("COPY 0 TO 1"))
//      val actor = Actors(settings1, codeLines).actors(0)
//      actor.memory(0) = 5
//      actor.executeStep()
//      assert(actor.memory(1) == 5)
    }

    "COPY #x TO 4" in {
//      val codeLines = Vector(Vector("COPY #x TO 4"))
//      val actor = Actors(settings1, codeLines).actors(0)
//      actor.context = List(INVALID -> Map("x" -> "123"))
//      actor.executeStep()
//      assert(actor.memory(4) == 3)
    }

    "ADD 0 TO x,x+1" in {
//      val codeLines = Vector(Vector("ADD #5 TO 0 OVERFLOW 1"))
//      val actor = Actors(settings1, codeLines).actors(0)
//      actor.memory(0) = 7
//      actor.executeStep()
//      assert(actor.memory(0) == 2)
//      assert(actor.memory(1) == 1)
    }

  }
