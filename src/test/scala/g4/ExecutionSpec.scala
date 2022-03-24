package g4

import util.chaining.scalaUtilChainingOps

class ExecutionSpec extends org.scalatest.freespec.AnyFreeSpec:
  val settings1: Settings = Settings(3, 4, 5)
  "The execution engine should process single commands" - {

    "COPY #6 TO 3" in {
      val codeLines = Vector("COPY #6 TO 3")
      val code = Compiler(settings1, codeLines).code
      val actors = Actors(settings1, code).tap(_.execute())
      assert(actors.memoryAsString == "~~~\n~~~\n~~~\n006")
    }

    "COPY #456 TO 3" in {
      val codeLines = Vector("COPY #456 TO 3")
      val code = Compiler(settings1, codeLines).code
      val actors = Actors(settings1, code).tap(_.execute())
      assert(actors.memoryAsString == "~~~\n~~~\n~~~\n456")
    }

    "COPY and ADD" in {
      val codeLines = Vector("COPY #456 TO 0", "ADD #456 TO 0 OVERFLOW 1")
      val code = Compiler(settings1, codeLines).code
      println(s"### $code")
      val actors = Actors(settings1, code).tap(_.execute())
      assert(actors.memoryAsString == "802\n011\n~~~\n~~~")
    }
  }
