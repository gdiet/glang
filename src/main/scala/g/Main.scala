package g

object Main extends ClassLogging
import Main.*

val script =
  """LOAD 0123456789012345
    |SAVE 0123456789012345 TO 0
    |ADD 0
    |loop CLEAR FLAG
    |IF CURRY SET FLAG
    |IF NOT FLAG GOTO end
    |ADD CURRY RIGHT
    |GOTO loop
    |end PRINT PROCESSOR""".stripMargin

@main def run(): Unit =
  val commands = Parser.parse(script)
  Executor.run(commands)

@main def plan(): Unit =
  val commands = Parser.parse(script)
  Planner.plan(commands)
