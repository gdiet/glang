package g4

import util.chaining.scalaUtilChainingOps

/*
add_unsigned: ADD 0 TO 1 OVERFLOW 2
<1> SET F FALSE; <-1> NOP
loop: <1> SET G FALSE; <-1> NOP
<-1> IF 2 > #0 THEN SET G TRUE; <1>: IF 2 > #0 THEN SET F TRUE
IF NOT G THEN RETURN
SHIFTLEFT 2
IF 2 > #0 THEN ADD #1 TO 1 OVERFLOW 2
GOTO loop
*/
class ExecutionSpec extends org.scalatest.freespec.AnyFreeSpec:
  val settings1: Settings = Settings(3, 4, 5)
  "The execution engine should process commands" - {

    "<-1> IF 2 > #0 THEN SET F TRUE; <1>: IF 2 > #0 THEN SET G TRUE" in {
      // TODO check, then implement
      val codeLines = Vector(
        "<1> SET F FALSE; <1> SET G FALSE; <-2> NOP",
        "COPY #456 TO 2",
        "<-1> IF 2 > #5 THEN SET F TRUE; <1>: IF 2 > #5 THEN SET G TRUE",
        "RETURN",
        "<-1> IF 2 < #5 THEN SET F FALSE; <1>: IF 2 < #5 THEN SET G TRUE",
      )
      val code = Compiler(settings1, codeLines).code
      val actors = Actors(settings1, code).tap(_.execute())
      assert(actors.flagsAsString == "+-??")
      // TODO run from line 4 then check
    }

    "loop: <1> SET F TRUE; <1> SET G FALSE; <-2> NOP" in {
      val codeLines = Vector("loop: <1> SET F TRUE; <1> SET G FALSE; <-2> NOP")
      val code = Compiler(settings1, codeLines).code
      val actors = Actors(settings1, code).tap(_.execute())
      assert(actors.flagsAsString == "+-??")
    }

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
