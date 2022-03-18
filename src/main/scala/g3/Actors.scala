package g3

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

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
