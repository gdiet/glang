package g4

import scala.util.matching.Regex

object Compiler:
  val log: org.slf4j.Logger = org.slf4j.LoggerFactory.getLogger(s"${getClass.getName}".replace("$", ""))
  
  val DIGIT_SOURCE_CONST  : Regex = """#(\d+)""".r
  val CMD_COPY            : Regex = """COPY (\S+) TO (\S+) (.*)""".r

class Compiler (settings: Settings, codeLines: Vector[String]):
  import Compiler.*
  import settings.DIGITS

  val code: Vector[Vector[String]] = codeLines.map(_ + " ").map { // Add space for simpler patterns
    case CMD_COPY(DIGIT_SOURCE_CONST(digits), target, comment) =>
      digits.map(digit => s"COPY #$digit TO $target $comment".trim).toVector
    case other =>
      Vector.fill(DIGITS)(other.trim)
  }
