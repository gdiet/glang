package g4

import scala.util.matching.Regex

object Compiler:
  val DIGIT_SOURCE_CONST  : Regex = """#(\S+)""".r
  val DIGIT_SOURCE_MEMORY : Regex = """(\S+)""".r
  
  val CMD_COPY            : Regex = """COPY (\S+) TO (\S+) .*""".r

class Compiler (settings: Settings, codeLines: Vector[String]):
  val code: Vector[Vector[String]] = ???
