package g4

import scala.util.matching.Regex

object Compiler:
  val log: org.slf4j.Logger = org.slf4j.LoggerFactory.getLogger(s"${getClass.getName}".replace("$", ""))

class Compiler (settings: Settings, codeLines: Vector[String]):
  import Compiler.*
  import settings.DIGITS

  val code: Vector[Vector[String]] = codeLines.map(_ + " ").map { // Add space for simpler patterns
    // prepared for case matching that will come eventually
    other =>
      Vector.fill(DIGITS)(other.trim)
  }
