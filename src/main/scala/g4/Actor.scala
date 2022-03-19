package g4

import scala.concurrent.Future
import scala.util.matching.Regex

class Actors(settings: Settings, codeLines: Vector[Vector[String]]):
  val actors: Vector[Actor] = (0 until settings.DIGITS).map(position =>
    Actor(settings, this, position, codeLines(position))
  ).toVector

  var flags: Map[Char, Boolean] = Map() // Flags F, G, H, I

object Actor:
  val DIGIT_SOURCE_DIGIT  : Regex = """#(\d)""".r
  val DIGIT_SOURCE_CONST  : Regex = """#(\S+)""".r
  val DIGIT_SOURCE_MEMORY : Regex = """\$(\S+)""".r

  val LABEL_CODE          : Regex = """(\w+:) (.*)""".r
  val COPY_TO_REG         : Regex = """COPY (\S+) TO §([A-D]) .*""".r
  val ADD_TO_REG_REG      : Regex = """ADD (\S+) TO §([A-D]),§([A-D]) .*""".r
  val SET_FLAG            : Regex = """SET !([F-I]) (TRUE|FALSE) .*""".r
  val IF_REG_OP_CONST_CODE: Regex = """IF §([A-D]) ([=><]) #(\d) THEN (.*)""".r
  val IF_FLAG_CODE        : Regex = """IF !([F-I]) THEN (.*)""".r
  val IFNOT_FLAG_CODE     : Regex = """IF NOT !([F-I]) THEN (.*)""".r

class Actor(settings: Settings, actors: Actors, position: Int, codeLines: Vector[String]):
  import Actor.*
  val log: org.slf4j.Logger = org.slf4j.LoggerFactory.getLogger(s"${getClass.getName}.$position")

  val memory: Array[Int] =
    Array.fill(settings.MEMORY)(INVALID)
  val jumpTargets: Map[String, Int] =
    codeLines.zipWithIndex.collect { case (LABEL_CODE(label, _), line) => label -> line }.toMap

  /** Stack frames containing the line number to return to and the frame's context map, which is
    * "replace string A with string B before processing" to support variables in function calls. */
  var context: List[(Int, Map[String, String])] = List(INVALID -> Map())
  var registers: Map[Char, Int] = Map() // Registers A, B, C, D
  var nextLine: Int = 0

  extension (in: String) def ctx: String = context.head._2.getOrElse(in, in)

  def executeStep(): Future[Unit] =
    val line = f"$nextLine%3d:"
    val codeLine = codeLines(nextLine)
    log.info(s"$line $codeLine")
    executeCode(line, codeLine)

  def advance(): Future[Unit] =
    nextLine += 1
    Future.unit

  def digitSource(string: String): Int = string match

    case DIGIT_SOURCE_DIGIT(digit)    => digit.ctx.toInt
    case DIGIT_SOURCE_CONST(source)   => source.ctx.takeRight(position + 1).take(1).toInt
    case DIGIT_SOURCE_MEMORY(address) => memory(address.ctx.toInt)

    case other =>
      log.error(s"digit source $other not recognized")
      0

  @annotation.tailrec
  final def executeCode(line: String, code: String): Future[Unit] = code + " " match // Add space for simpler patterns

    case LABEL_CODE(_, fragment) =>
      executeCode(line, fragment)

    case COPY_TO_REG(source, register) =>
      val sourceDigit = digitSource(source)
      log.info(s"$line COPY #$sourceDigit TO §$register")
      registers += register.head -> sourceDigit
      advance()

    case ADD_TO_REG_REG(source, reg1, reg2) =>
      log.info(s"$line ADD #${digitSource(source)} TO §$reg1,§$reg2")
      val sum = registers(reg1.head) + digitSource(source)
      registers += reg1.head -> sum % 10
      registers += reg2.head -> sum / 10
      advance()

    case SET_FLAG(flag, value) =>
      log.info(s"$line SET_FLAG(!$flag, $value)")
      actors.flags += flag.head -> value.toBoolean
      advance()

    case IF_REG_OP_CONST_CODE(register, operator, constant, fragment) =>
      operator match
        case ">" if registers(register.head) > constant.toInt => executeCode(line, fragment)
        case "<" if registers(register.head) < constant.toInt => executeCode(line, fragment)
        case "=" if registers(register.head) == constant.toInt => executeCode(line, fragment)
        case other => advance()

    case IF_FLAG_CODE(flag, fragment) =>
      if actors.flags(flag.head) then executeCode(line, fragment) else advance()

    case IFNOT_FLAG_CODE(flag, fragment) =>
      if !actors.flags(flag.head) then executeCode(line, fragment) else advance()

    case other =>
      log.info(s"$line NOP: '$code'")
      advance()
