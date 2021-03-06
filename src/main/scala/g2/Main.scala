package g2

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex

@main def main(): Unit =
  val script = """
FUNCTION add_unsigned $x $y $z
LOAD $x TO A
ADD  $y TO A,B
CLEAR F
loop: CLEAR G
0: IF B > 0 THEN SET F; 1..N: IF B > 0 THEN SET G
IF NOT G THEN RETURN
SHIFTLEFT B
IF B > 0 THEN ADD #1 TO A,B
GOTO loop
RETURN // TODO remove - just necessary as long as the rest is not yet complete
END_FUNCTION

SAVE #0678 TO $0
SAVE #9876 TO $1
CALL add_unsigned $0 $1 $2
// END
"""

  val lines = script.linesIterator.toVector
  println("\nCompilation:")
  val commands = compile(lines)
  println("\nExecution:")
  val threadPool: ExecutorService = Executors.newFixedThreadPool(DIGITS)
  Executor(lines, commands, ExecutionContext.fromExecutor(threadPool)).execute(0)
  threadPool.shutdown()
// TODO write an executor for the single actor, and one controller for the flow control
//  exec(lines, State())

def strip(line: String): String =
  line
    .replaceAll(raw"//.*", "") // cut off comments
    .replaceAll(raw"\s+", " ").trim // clean up spaces

object mat:
  val SAVE: Regex = raw"SAVE (\S+) TO (\S+)".r
  val LOAD: Regex = raw"LOAD (\S+) TO (\S+)".r
  val ADD: Regex = raw"ADD (\S+) TO (\S),(\S)".r
  val FUNCTION: Regex = raw"FUNCTION (\S+)(.*)".r
  val END_FUNCTION: Regex = raw"END_FUNCTION".r
  val CALL: Regex = raw"CALL (\S+)(.*)".r
  val RETURN: Regex = raw"RETURN".r
  val REGISTER: Regex = raw"([ABCD])".r
  val NAME: Regex = raw"([a-z_]*)".r
  val DIGIT: Regex = raw"(#\S)".r
  /** before context replacement */
  val CONSTANT: Regex = raw"(#\S+)".r
  /** after context replacement */
  val CONST: Regex = raw"(\d{$DIGITS})".r
  /** before context replacement */
  val ADDRESS: Regex = raw"(\$$\S+)".r
  /** after context replacement */
  val ADDR: Regex = raw"(\d+)".r
  object S { def unapply(string: String): Option[String] = Some(string.drop(1)) }

def compile(lines: Vector[String]): Vector[Vector[String]] =
  (0 until DIGITS).map { actor =>
    println(s"\nActor $actor:")
    lines.map(strip).map { line =>
      compile(actor, line).tap(println)
    }
  }.toVector

def compile(actor: Int, line: String): String =
  import mat.*
  line match

    // SAVE #0678 TO $0
    case SAVE(CONSTANT(S(CONST(constant))), ADDRESS(S(ADDR(to)))) =>
      raw"SAVE #${constant(actor)} TO $$$to"

    // LOAD $0 TO A
    case LOAD(ADDRESS(address), REGISTER(to)) =>
      raw"LOAD $address TO $to"

    // ADD $0 TO A,B
    case ADD(ADDRESS(S(ADDR(address))), REGISTER(to), REGISTER(overflow)) =>
      raw"ADD $$$address TO $to,$overflow"

    // FUNCTION add_unsigned $x $y $z
    case FUNCTION(NAME(name), params) =>
      raw"FUNCTION ${(name + params).strip}"

    // CALL add_unsigned $0 $1 $2
    case CALL(NAME(name), params) =>
      raw"CALL ${(name + params).strip}"

    // RETURN
    case RETURN() =>
      raw"RETURN"

    // END_FUNCTION
    case END_FUNCTION() =>
      raw"END_FUNCTION"

    // empty lines can be ignored
    case "" =>
      ""

    case other =>
      s"** unknown command '$other' **"


final class Executor(lines: Vector[String], commands: Vector[Vector[String]], implicit val ec: ExecutionContext):
  require(commands.forall(_.size == lines.size))
  require(commands.size == DIGITS)

  var functions: Map[String, Int] = Map()
  var stack: List[Int] = List()
  val actors: Vector[Actor] = (0 until DIGITS).map(Actor(this, _)).toVector

  @annotation.tailrec
  def execute(lineNo: Int): Unit = if lineNo < lines.size then
    val line = lines(lineNo)
    val cmds = commands.map(_(lineNo))
    println(f"$lineNo%3d $line")

    import mat.*
    cmds(0) match
      // FUNCTION add_unsigned $x $y $z
      case FUNCTION(NAME(name), params) =>
        val endFunctionAt = commands(0).indexWhere(END_FUNCTION.matches, lineNo + 1)
        execute(endFunctionAt + 1)

      // CALL add_unsigned $0 $1 $2
      case CALL(NAME(name), params) =>
        // FIXME dupliacte code
        val future = Future.sequence(actors.map { actor =>
          Thread.sleep(100) // Ensures the debug output is in a nice order
          Future(actor.execute(cmds(actor.digit)))
        })
        Await.ready(future, Duration.Inf)
        ???

      case "" =>
        execute(lineNo + 1)

      case cmd =>
        val future = Future.sequence(actors.map { actor =>
          Thread.sleep(100) // Ensures the debug output is in a nice order
          Future(actor.execute(cmds(actor.digit)))
        })
        Await.ready(future, Duration.Inf)
        execute(lineNo + 1)


class Actor(executor: Executor, val digit: Int):
  val mem: Array[Int] = Array.fill(MEMORY)(-1)
  val flags: Map[Char, Boolean] = Map('F' -> false, 'G' -> false, 'H' -> false, 'I' -> false)
  val reg: Map[Char, Int] = Map('A' -> -1, 'B' -> -1, 'C' -> -1, 'D' -> -1)
  val ctx: List[Map[String, String]] = List(Map())

  def out(msg: String): Unit = System.err.println(msg)

  def execute(cmd: String): Unit =
    import mat.*
    cmd match

      // SAVE #6 TO $0
      case SAVE(DIGIT(S(digit)), ADDRESS(S(ADDR(to)))) =>
        mem(to.toInt) = digit.toInt

      case other =>
        out(s"Actor $digit - not implemented: $other")


//@annotation.tailrec
//def exec(lines: Vector[String], state: State): Unit =
//  if lines.size > state.line then
//    val line = lines(state.line)
//    state.out()
//    println(f"${state.line}%02d $line")
//    exec(lines, strip(line), state)
//    exec(lines, state)
//
//def exec(lines: Vector[String], currentLine: String, s: State): Unit =
//  import mat.*
//  import s.*
//  import matchers.*
//  currentLine match
//    case "" =>
//      line += 1
//
//    // SAVE #0678 TO $0
//    case SAVE(constant(constant), address(to)) =>
//      mem(to) = constant
//      line += 1
//
//    // LOAD $0 TO A
//    case LOAD(address(from), REGISTER(to)) =>
//      reg(to) = mem(from)
//      line += 1
//
//    // ADD $0 TO A,B
//    case ADD(address(from), REGISTER(to), REGISTER(overflow)) =>
//      val (a,b) = reg(to).zip(mem(from)).map(_+_).map(i => i % 10 -> i / 10).unzip
//      reg(to) = a
//      reg(overflow) = b
//      line += 1
//
//    // FUNCTION add_unsigned $x $y $z
//    case FUNCTION(NAME(name), rawParams) =>
//      val params = rawParams.split(raw"\s+").filterNot(_.isEmpty)
//      require(params.forall(_.startsWith("$")))
//      line += 1
//      functions += name -> FuncDefinition(line, params.toSeq)
//      while strip(lines(line)) != "END_FUNCTION"
//      do { println(s"FN ${lines(line)}"); line += 1 }
//      println(s"FN ${lines(line)}")
//      line += 1
//
//    // CALL add_unsigned $0 $1 $2
//    case CALL(NAME(name), rawParams) =>
//      require(stack.size < STACK, s"Stack overflow")
//      val params = rawParams.split(raw"\s+").filterNot(_.isEmpty).toSeq
//      require(params.forall(_.startsWith("$")))
//      val function = functions(name)
//      val functionParameters = function.parameters.zip(params).toMap
//      stack :+= Stackframe(function.line, functionParameters.to(mutable.Map))
//      Thread.sleep(500)
//
//    // RETURN
//    case RETURN() =>
//      require(stack.nonEmpty, s"Stack underflow")
//      stack = stack.dropRight(1)
//      line += 1
//
//    case other =>
//      println(s"** unknown command '$other' **")
//      line += 1
//
//class FuncDefinition(val line: Int, val parameters: Seq[String])
//
//class Stackframe(
//  var line: Int = 0,
//  val context: mutable.Map[String, String] = mutable.Map[String, String]()
//)
//
//class State:
//  def out(): Unit =
//    def memOut(m: Int): Int | String = if m < 0 then "#" else m
//    def l = line
//    def r = s"${reg("A").map(memOut).mkString} ${reg("B").map(memOut).mkString} ${reg("C").map(memOut).mkString} ${reg("D").map(memOut).mkString}"
//    def m = mem.map(_.map(memOut).mkString).mkString(" ")
//    println(s"L $l / R $r / M $m")
//  private def invalid = List.fill(DIGITS)(-1)
//
//  val mem: Array[List[Int]] = Array.fill(MEMORY)(invalid)
//  val reg: mutable.Map[String, List[Int]] = mutable.Map("A" -> invalid, "B" -> invalid, "C" -> invalid, "D" -> invalid)
//  var stack: Vector[Stackframe] = Vector(Stackframe())
//  val functions: mutable.Map[String, FuncDefinition] = mutable.Map()
//  def context: mutable.Map[String, String] = stack.last.context
//  def line: Int = stack.last.line
//  def line_=(line: Int): Unit = stack.last.line = line
//
//  def unapply(string: String): Option[String] = Some(context.getOrElse(string, string).drop(1))
//  val self: State = this
//  object matchers:
//    object constant:
//      def unapply(string: String): Option[List[Int]] = string match
//        case mat.CONSTANT(self(mat.CONST(raw))) => Some(raw.getBytes.toList.map(_ - 48))
//        case other => None
//    object address:
//      def unapply(string: String): Option[Int] = string match
//        case mat.ADDRESS(self(mat.ADDR(raw))) => raw.toIntOption
//        case other => None
