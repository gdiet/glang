package g3

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.util.matching.Regex

class Actors(settings: Settings) extends AutoCloseable:
  import settings.*
  private val threadPool = Executors.newFixedThreadPool(DIGITS)
  private implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(threadPool)
  override def close(): Unit = threadPool.shutdown()

  var flags: Map[Char, Boolean] = Map('F' -> false, 'G' -> false, 'H' -> false, 'I' -> false)
  val actors: Seq[Actor] = (0 until DIGITS).map(Actor(settings, _, this)).toVector

class Actor(settings: Settings, position: Int, actors: Actors):
  import settings.*
  val memory: Array[Int] = Array.fill(MEMORY)(INVALID)
  var registers: Map[Char, Int] = Map('A' -> INVALID, 'B' -> INVALID, 'C' -> INVALID, 'D' -> INVALID)
  /** The head of the context contains the current context, it's a list because the tail contains the contexts
    * of the calling stack frames. The current context is a "replace string A with string B before processing" map
    * to support variables in calls. */
  var context: List[Map[String, String]] = List(Map())

  def execute(command: String): Unit = command match
    case other => /* ignore */

object Manager:
  // (loop):
  // (loop): SOME_COMMAND
  val gotoTargetLine: Regex = "([a-z_]+):.*".r
  // FUNCTION (add_unsigned): ()
  // FUNCTION (add_unsigned): ($x $y $z)
  val functionDefinitionLine: Regex = "FUNCTION ([a-z_]+): *(.*)".r
  val endFunctionLine: Regex = "END_FUNCTION".r
  val returnLine: Regex = "(|[a-z_]+: *)RETURN".r
  val someCommandLine: Regex = "([a-z_]+:){0,1} *(.*)".r

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

  /** @param context Stack frames containing the line number to return to and the frame's context. */
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

        case someCommandLine(_, other) =>
          log.info(s"other command: $other")
          execute(lineNumber + 1, context)

        case other =>
          log.info(s"UNEXPECTED $other")
          execute(lineNumber + 1, context)
