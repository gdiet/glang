package g

object Main extends ClassLogging
import Main.*

@main def run(): Unit =
  val script =
    """LOAD 0123456789012345
      |SAVE 0123456789012345 TO 1
      |""".stripMargin
  Parser.parse(script)