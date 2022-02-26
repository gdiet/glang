package g3

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.util.matching.Regex

class Actors(settings: Settings) extends AutoCloseable with ClassLogging:
  import settings.*
  private val threadPool = Executors.newFixedThreadPool(DIGITS)
  private implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(threadPool)
  override def close(): Unit = threadPool.shutdown()

  var flags: Map[Char, Boolean] = Map('F' -> false, 'G' -> false, 'H' -> false, 'I' -> false)
  val actors: Seq[Actor] = (0 until DIGITS).map(Actor(settings, _, this)).toVector

  def execute(command: String): Unit =
    log.info(s"executing: $command")

class Actor(settings: Settings, position: Int, actors: Actors):
  import settings.*
  val memory: Array[Int] = Array.fill(MEMORY)(INVALID)
  var registers: Map[Char, Int] = Map('A' -> INVALID, 'B' -> INVALID, 'C' -> INVALID, 'D' -> INVALID)

  def execute(command: String): Unit = command match
    case other => /* ignore */

object Manager:
  // (loop):
  // (loop): SOME_COMMAND
  val gotoTargetLine: Regex = "([a-z_]+):.*".r
  // FUNCTION (add_unsigned): ()
  // FUNCTION (add_unsigned): ($x $y $z)
  val functionDefinitionLine: Regex = "FUNCTION ([a-z_]+): *(.*)".r
  // END_FUNCTION
  val endFunctionLine: Regex = "END_FUNCTION".r
  // (loop:) (SOME_COMMAND)
  // ()(SOME_COMMAND)
  val someCommandLine: Regex = "([a-z_]+:){0,1} *(.*)".r
  // SAVE (#1234) TO ($3)
  val saveConstantToAddress: Regex = "SAVE (.*) TO (.*)".r

class Manager(lines: Seq[String], settings: Settings = Settings()) extends AutoCloseable with ClassLogging:
  import Manager.*
  val actors: Actors = Actors(settings)
  override def close(): Unit = actors.close()

  log.info("Pass 1: goto targets and function definitions")

  val gotoTargets: Map[String, Int] = lines.zipWithIndex.collect {
    case (gotoTargetLine(targetName), lineNumber) => targetName -> lineNumber
  }.toMap
  log.info(s"goto targets: $gotoTargets")

  val functionDefinitions: Map[String, Int] = lines.zipWithIndex.collect {
    case (functionDefinitionLine(functionName, _), lineNumber) => functionName -> lineNumber
  }.toMap
  log.info(s"function definitions: $functionDefinitions")

  def execute(startAtLineNumber: Int): Unit =
    log.info(s"Pass 2: Execution, starting at line $startAtLineNumber")
    execute(startAtLineNumber, List((lines.size, Map())))

  /** @param context Stack frames containing the line number to return to and the frame's context.
    *                A context is a "replace string A with string B before processing" map
    *                to support variables in function calls.*/
  @annotation.tailrec
  private def execute(lineNumber: Int, context: List[(Int, Map[String, String])]): Unit =
    if lines.size > lineNumber then
      val line = lines(lineNumber)
      log.info(f"executing: $lineNumber%2d $line")
      line.replaceAll("// .*", "").trim match

        case functionDefinitionLine(functionName, parameters) =>
          log.info(s"function definition: $functionName, parameters $parameters")
          val lineWithEndFunction = lines.indexWhere(endFunctionLine.matches, lineNumber)
          execute(lineWithEndFunction + 1, context)

        case someCommandLine(_, "") =>
          log.info("empty line")
          execute(lineNumber + 1, context)

        case someCommandLine(_, rawCommand) =>
          log.info(s"command: $rawCommand")
          val contextCommand = context.head._2.foldLeft(rawCommand){
            case (command, (find, replace)) => command.replaceAll(s"\\Q$find\\E", replace)
          }
          actors.execute(contextCommand)
          execute(lineNumber + 1, context)

        case other =>
          log.info(s"UNEXPECTED $other")
          execute(lineNumber + 1, context)
