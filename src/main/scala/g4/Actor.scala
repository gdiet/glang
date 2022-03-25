package g4

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.matching.Regex

class Actors(settings: Settings, codeLines: Vector[Vector[String]]):
  val actors: Vector[Actor] = (0 until settings.DIGITS).map(position =>
    Actor(settings, this, position, codeLines.map(_(position)))
  ).toVector

  def flagAsString(flag: Char): String =
    flags.get(flag).map { case true => "+"; case false => "-" }.getOrElse("?")

  def flagsAsString: String =
    flagAsString('F') + flagAsString('G') + flagAsString('H') + flagAsString('I')

  def memoryAsString: String =
    (0 until settings.MEMORY).map(addressAsString).mkString("\n")

  def addressAsString(address: Int): String =
    actors.reverse.map(_.memory(address)).map {
      case n if n < 0 => "~"
      case n if n > 9 => s"!$n!"
      case n => s"$n"
    }.mkString

  var flags: Map[Char, Boolean] = Map() // Flags F, G, H, I

  @annotation.tailrec
  final def execute(): Unit =
    if actors(0).hasMoreSteps then
      require(actors.forall(_.hasMoreSteps))
      import scala.concurrent.ExecutionContext.Implicits.global
      Await.ready(Future.sequence(actors.map(_.executeStep())), Duration.Inf)
      execute()
    else
      require(actors.forall(!_.hasMoreSteps))


object Actor:
  val TARGET_OFFSET       : Regex = """(\S+)\+(\d+)""".r
  val TARGET              : Regex = """(\S+)""".r

  val DIGIT_SOURCE_CONST  : Regex = """#(\S+)""".r
  val DIGIT_SOURCE_MEMORY : Regex = """(\S+)""".r

  val LABEL_CODE          : Regex = """(\w+:) (.*)""".r
  val CMD_COPY            : Regex = """COPY (\S+) TO (\S+) .*""".r
  val CMD_ADD             : Regex = """ADD (\S+) TO (\S+) OVERFLOW (\S+) .*""".r
  val SET_FLAG            : Regex = """SET ([F-I]) (TRUE|FALSE) .*""".r
  val IF_REG_OP_CONST_CODE: Regex = """IF §([A-D]) ([=><]) #(\d) THEN (.*)""".r
  val IF_FLAG_CODE        : Regex = """IF !([F-I]) THEN (.*)""".r
  val IFNOT_FLAG_CODE     : Regex = """IF NOT !([F-I]) THEN (.*)""".r

class Actor(settings: Settings, actors: Actors, position: Int, codeLines: Vector[String]):
  import Actor.*
  import settings.*

  val log: org.slf4j.Logger = org.slf4j.LoggerFactory.getLogger(s"${getClass.getName}.$position")

  val memory: Array[Int] =
    Array.fill(MEMORY)(INVALID)
  val jumpTargets: Map[String, Int] =
    codeLines.zipWithIndex.collect { case (LABEL_CODE(label, _), line) => label -> line }.toMap
  var returnTargets: List[Int] =
    List(INVALID)

  var nextLine: Int = 0
  def hasMoreSteps: Boolean = nextLine >= 0 && nextLine < codeLines.size

  def executeStep(): Future[Unit] =
    val line = f"$nextLine%3d:"
    val codeLine = codeLines(nextLine)
    log.info(s"$line $codeLine")
    executeCode(line, codeLine)

  def advance(): Future[Unit] =
    nextLine += 1
    Future.unit

  def digitSource(string: String): Int = string match
    case DIGIT_SOURCE_CONST(source)   =>
      ("0" * (DIGITS - source.length) + source).takeRight(position + 1).take(1).toInt
    case DIGIT_SOURCE_MEMORY(address) =>
      memory(address.toInt)
    case other =>
      log.error(s"digit source $other not recognized"); 0

  @annotation.tailrec
  final def executeCode(line: String, code: String): Future[Unit] = code + " " match // Add space for simpler patterns

    case LABEL_CODE(_, fragment) =>
      executeCode(line, fragment)

    case CMD_COPY(source, target) =>
      val sourceDigit = digitSource(source)
      val targetAddress = target.toInt
      log.info(s"$line COPY #$sourceDigit TO $targetAddress")
      memory(targetAddress) = sourceDigit
      advance()

    case CMD_ADD(source, target1, target2) =>
      val sourceDigit = digitSource(source)
      val target1Address = target1.toInt
      val target2Address = target2.toInt
      log.info(s"$line ADD #$sourceDigit TO $target1Address,$target2Address")
      val sum = memory(target1Address) + sourceDigit
      memory(target1Address) = sum % 10
      memory(target2Address) = sum / 10
      advance()

    case SET_FLAG(flag, value) =>
      log.info(s"$line SET $flag $value")
      actors.flags += flag.head -> value.toBoolean
      advance()

    case IF_REG_OP_CONST_CODE(register, operator, constant, fragment) =>
      ???
//      operator match
//        case ">" if registers(register.head) > constant.toInt => executeCode(line, fragment)
//        case "<" if registers(register.head) < constant.toInt => executeCode(line, fragment)
//        case "=" if registers(register.head) == constant.toInt => executeCode(line, fragment)
//        case other => advance()

    case IF_FLAG_CODE(flag, fragment) =>
      if actors.flags(flag.head) then executeCode(line, fragment) else advance()

    case IFNOT_FLAG_CODE(flag, fragment) =>
      if !actors.flags(flag.head) then executeCode(line, fragment) else advance()

    case other =>
      log.info(s"$line NOP: '$code'")
      advance()
