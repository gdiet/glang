package g4

import scala.util.matching.Regex

object Compiler:
  val log: org.slf4j.Logger = org.slf4j.LoggerFactory.getLogger(s"${getClass.getName}".replace("$", ""))

  val LABEL_CODE: Regex = """(\w+:) (.*)""".r
  val SET_FLAG  : Regex = """SET ([F-I]) (TRUE|FALSE) .*""".r

class Compiler (settings: Settings, codeLines: Vector[String]):
  import Compiler.*
  import settings.DIGITS

  val code: Vector[Vector[String]] = codeLines.map(compile)

  def compile(line: String): Vector[String] = line + " " match { // Add space for simpler patterns
    case LABEL_CODE(label, code) =>
      compile(code).map(s"$label " + _)
    case SET_FLAG(flag, value) =>
      s"SET $flag $value" +: Vector.fill(DIGITS - 1)("NOP")
    case other =>
      Vector.fill(DIGITS)(other.trim)
  }
