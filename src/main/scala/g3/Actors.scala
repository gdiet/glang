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
  val gotoTargetLine: Regex = "^([a-z_]+):.*".r
  val functionDefinitionLine: Regex = "^FUNCTION ([a-z_]+):.*".r

class Manager(settings: Settings, lines: Seq[String]) extends AutoCloseable with ClassLogging:
  import Manager.*
  val actors: Actors = Actors(settings)
  override def close(): Unit = actors.close()
  
  val gotoTargets: Map[String, Int] = lines.zipWithIndex.collect {
    case (gotoTargetLine(targetName), lineNumber) => targetName -> lineNumber
  }.toMap
  log.info(s"goto targets: $gotoTargets")
  
  val functionDefinitions: Map[String, Int] = lines.zipWithIndex.collect {
    case (functionDefinitionLine(functionName), lineNumber) => functionName -> lineNumber
  }.toMap
  log.info(s"function definitions: $functionDefinitions")


//  /** @return the next line to execute. */
//  def execute(line: Int): Int =
//    log.debug(s"execute: $line")
//    line match
//      case other => line + 1
