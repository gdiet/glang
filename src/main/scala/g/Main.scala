package g

object Main extends ClassLogging
import Main.*

@main def run(): Unit =
  val script =
    """
      |LOAD 0123456789012345
      |PRINT ACC
    """.stripMargin
/*
      |DEF addition x ADDRESS
      |ADD x
      |IF OVERFLOW +1 ADD 1
      |CLEAR OVERFLOW $REGISTER_WIDTH
      |IF OVERFLOW +0 SET FLAG
      |IFNOT FLAG GOTO end
      |loop:
      |CLEAR FLAG
      |IF OVERFLOW +1 ADD 1
      |IF OVERFLOW +0 SET FLAG
      |IF FLAG GOTO loop
      |end:
      |RETURN
      |
      |LOAD 0123456789012345
      |SAVE 0123456789012345 TO 1
      |CALL addition 1
      |PRINT
*/
  val commands = Parser.parse(script)
  Executor.run(commands)