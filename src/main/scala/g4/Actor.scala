package g4

import g4.Actor.{ADD_MEM_TO_REG_REG, COPY_MEM_TO_REG, SET_FLAG}

import scala.concurrent.Future
import scala.util.matching.Regex

class Actors(settings: Settings):
  val actors: Vector[Actor] = (0 until settings.DIGITS).map(Actor(settings, this, _)).toVector

  var flags: Map[Char, Boolean] = Map('F' -> false, 'G' -> false, 'H' -> false, 'I' -> false)

object Actor:
  val COPY_MEM_TO_REG: Regex = """COPY (\$\S+) TO §(\S) .*""".r
  val ADD_MEM_TO_REG_REG: Regex = """ADD (\$\S+) TO §(\S),§(\S) .*""".r
  val SET_FLAG: Regex = """SET !(\S) (TRUE|FALSE) .*""".r

class Actor(settings: Settings, actors: Actors, position: Int):
  val log: org.slf4j.Logger = org.slf4j.LoggerFactory.getLogger(s"${getClass.getName}.$position")
  val memory: Array[Int] = Array.fill(settings.MEMORY)(INVALID)

  /** Stack frames containing the line number to return to and the frame's context map, which is
    * "replace string A with string B before processing" to support variables in function calls. */
  var context: List[(Int, Map[String, String])] = List(INVALID -> Map())
  var registers: Map[Char, Int] = Map() // Registers A, B, C, D
  var codeLines: Vector[String] = Vector()
  var nextLine: Int = 0

  def ctx(in: String): String = context.head._2.getOrElse(in, in)

  def executeStep(): Future[Unit] =
    val line = f"$nextLine%d3:"
    val code = codeLines(nextLine)
    log.debug(s"$line $code")
    code + " " match // Add space for simpler patterns

      case COPY_MEM_TO_REG(address, register) =>
        log.info(s"$line COPY_MEM_TO_REG(${ctx(address)}, §$register)")
        registers += register.head -> memory(ctx(address).tail.toInt)
        nextLine += 1
        Future.unit

      case ADD_MEM_TO_REG_REG(address, reg1, reg2) =>
        log.info(s"$line ADD_MEM_TO_REG_REG(${ctx(address)}, §$reg1, §$reg2)")
        val sum = registers(reg1.head) + memory(ctx(address).tail.toInt)
        registers += reg1.head -> sum % 10
        registers += reg2.head -> sum / 10
        Future.unit

      case SET_FLAG(flag, value) =>
        log.info(s"$line SET_FLAG(!$flag, $value)")
        actors.flags += flag.head -> value.toBoolean
        Future.unit
        
      case other =>
        log.info(s"$line NOP")
        Future.unit
