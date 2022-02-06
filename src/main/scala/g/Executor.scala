package g

object Executor extends ClassLogging:

  def run(commands: List[Command]): Unit =
    run(ProcessorState(), commands)

  def run(state: ProcessorState, commands: List[Command]): Unit =
    info("Starting execution...")
    commands.iterator.tapEach(c => info(s"$c")).foreach {
      case NOP(_) => /**/
      case LOAD(constant) => state.acc = constant.value
      case PRINTACC =>
        info(s"Accumulator: ${state.acc.mkString}")
        info(s"Curry flags: ${state.curry.map(c => if c then "*" else " ").mkString}")
      case PRINTADDRESS(address) => info(s"Memory at ${address.position} is ${state.mem(address.position).mkString.replaceAll("-\\d", "#")}")
      case SAVECONST(constant, to) => state.mem(to.position) = constant.value
      case ADDADDRESS(address) =>
        val (curry, acc) = state.acc.zip(state.mem(address.position))
          .map { case (a,b) => (a+b > 9, (a+b) % 10) }.unzip
        state.curry = curry; state.acc = acc
      case other => warn(s"No executor for: $other")
    }
    info("Execution finished.")

class ProcessorState:
  var acc: List[Int] = Nil
  var curry: List[Boolean] = Nil
  val mem: Array[List[Int]] = Array.fill(MEMORY_SIZE)(List.fill(REGISTER_WIDTH)(-1))
