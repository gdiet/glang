package g4

import scala.util.matching.Regex

object Compiler:
  val log: org.slf4j.Logger = org.slf4j.LoggerFactory.getLogger(s"${getClass.getName}".replace("$", ""))

  val SET_FLAG: Regex = """SET ([F-I]) (TRUE|FALSE) .*""".r

class Compiler (settings: Settings, codeLines: Vector[String]):
  import Compiler.*
  import settings.DIGITS

  val code: Vector[Vector[String]] = codeLines.map(_ + " ").map { // Add space for simpler patterns
    case SET_FLAG(flag, value) =>
      s"SET $flag $value" +: Vector.fill(DIGITS - 1)("NOP")
    case other =>
      Vector.fill(DIGITS)(other.trim)
  }
