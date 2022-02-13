package g2

import scala.collection.mutable
import scala.util.matching.Regex

@main def main(): Unit =
  val script = """
SAVE #0678 TO $0
LOAD $0    TO A
ADD  $0    TO A,B
FUNCTION add_unsigned $x $y $z
LOAD $x TO A
RETURN
END_FUNCTION
CALL add_unsigned $0 $0 $1
LOAD $0    TO A
// END
"""

  val lines = script.linesIterator.toVector
  exec(lines, State())
  compile(lines)

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
  /** before context replacement */
  val CONSTANT: Regex = raw"(#\S+)".r
  /** after context replacement */
  val CONST: Regex = raw"(\d{$DIGITS})".r
  /** before context replacement */
  val ADDRESS: Regex = raw"(\$$\S+)".r
  /** after context replacement */
  val ADDR: Regex = raw"(\d+)".r

def compile(lines: Vector[String]): Unit =
  println("\nCompilation:")
  (0 until DIGITS).foreach { actor =>
    println(s"\nActor $actor:")
    lines.map(strip).filterNot(_.isEmpty).foreach(compile(actor))
  }

def compile(actor: Int)(line: String): Unit =
  object S { def unapply(string: String): Option[String] = Some(string.drop(1)) }
  import mat._
  line match

    // SAVE #0678 TO $0
    case SAVE(CONSTANT(S(CONST(constant))), ADDRESS(S(ADDR(to)))) =>
      println(raw"SAVE #${constant(actor)} TO $$$to")

    // LOAD $0 TO A
    case LOAD(ADDRESS(S(ADDR(address))), REGISTER(to)) =>
      println(raw"LOAD $$$address TO $to")

    // ADD $0 TO A,B
    case ADD(ADDRESS(S(ADDR(address))), REGISTER(to), REGISTER(overflow)) =>
      println(raw"ADD $$$address TO $to,$overflow")

    case other =>
      println(s"** unknown command '$other' **")

@annotation.tailrec
def exec(lines: Vector[String], state: State): Unit =
  if lines.size > state.line then
    val line = lines(state.line)
    state.out()
    println(f"${state.line}%02d $line")
    exec(lines, strip(line), state)
    exec(lines, state)

def exec(lines: Vector[String], currentLine: String, s: State): Unit =
  import mat.*
  import s.*
  import matchers.*
  currentLine match
    case "" =>
      line += 1

    // SAVE #0678 TO $0
    case SAVE(constant(constant), address(to)) =>
      mem(to) = constant
      line += 1

    // LOAD $0 TO A
    case LOAD(address(from), REGISTER(to)) =>
      reg(to) = mem(from)
      line += 1

    // ADD $0 TO A,B
    case ADD(address(from), REGISTER(to), REGISTER(overflow)) =>
      val (a,b) = reg(to).zip(mem(from)).map(_+_).map(i => i % 10 -> i / 10).unzip
      reg(to) = a
      reg(overflow) = b
      line += 1

    case FUNCTION(NAME(name), rawParams) =>
      val params = rawParams.split(raw"\s+").filterNot(_.isEmpty)
      require(params.forall(_.startsWith("$")))
      line += 1
      functions += name -> FuncDefinition(line, params.toSeq)
      while strip(lines(line)) != "END_FUNCTION"
      do { println(s"FN ${lines(line)}"); line += 1 }
      println(s"FN ${lines(line)}")
      line += 1

    case CALL(NAME(name), rawParams) =>
      val params = rawParams.split(raw"\s+").filterNot(_.isEmpty)
      require(params.forall(_.startsWith("$")))
      val function = functions(name)
      stack :+= Stackframe(function.line, function.parameters)

    case other =>
      println(s"** unknown command '$other' **")
      line += 1

class FuncDefinition(val line: Int, val parameters: Seq[String])

class Stackframe(
  var line: Int = 0,
  val context: mutable.Map[String, String] = mutable.Map[String, String]()
)

class State:
  def out(): Unit =
    def memOut(m: Int): Int | String = if m < 0 then "#" else m
    def l = line
    def r = s"${reg("A").map(memOut).mkString} ${reg("B").map(memOut).mkString} ${reg("C").map(memOut).mkString} ${reg("D").map(memOut).mkString}"
    def m = mem.map(_.map(memOut).mkString).mkString(" ")
    println(s"L $l / R $r / M $m")
  private def invalid = List.fill(DIGITS)(-1)

  val mem: Array[List[Int]] = Array.fill(MEMORY)(invalid)
  val reg: mutable.Map[String, List[Int]] = mutable.Map("A" -> invalid, "B" -> invalid, "C" -> invalid, "D" -> invalid)
  var stack: Vector[Stackframe] = Vector(Stackframe())
  val functions: mutable.Map[String, FuncDefinition] = mutable.Map()
  def context: mutable.Map[String, String] = stack.last.context
  def line: Int = stack.last.line
  def line_=(line: Int): Unit = stack.last.line = line

  def unapply(string: String): Option[String] = Some(context.getOrElse(string, string).drop(1))
  val self: State = this
  object matchers:
    object constant:
      def unapply(string: String): Option[List[Int]] = string match
        case mat.CONSTANT(self(mat.CONST(raw))) => Some(raw.getBytes.toList.map(_ - 48))
        case other => None
    object address:
      def unapply(string: String): Option[Int] = string match
        case mat.ADDRESS(self(mat.ADDR(raw))) => raw.toIntOption
        case other => None
