package g4

import util.chaining.scalaUtilChainingOps

class ExecutionSpec extends org.scalatest.freespec.AnyFreeSpec:
  val settings1: Settings = Settings(3, 4, 5)
  "The execution engine should process single commands" - {

    "COPY #456 TO 3" in {
      val codeLines = Vector("COPY #456 TO 3")
      val code = Compiler(settings1, codeLines).code
      val actors = Actors(settings1, code).tap(_.execute())
      assert(actors.memoryAsString == "~~~\n~~~\n~~~\n456")
    }
  }
