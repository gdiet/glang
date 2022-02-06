package g

object Executor extends ClassLogging:

  def run(commands: List[Command]): Unit =
    run(ProcessorState(), commands)

  def run(state: ProcessorState, commands: List[Command]): Unit =
    info("Starting execution...")
    commands.iterator.tapEach(c => info(s"$c")).foreach {
      case NOP(_) => /**/
      case LOAD(constant) => state.acc = constant.value
      case PRINTACC => info(s"Accumulator is: ${state.acc.mkString}")
      case PRINTADDRESS(address) => info(s"Memory at ${address.position} is ${state.mem(address.position).mkString.replaceAll("-\\d", "#")}")
      case SAVECONST(constant, to) => state.mem(to.position) = constant.value
      case other => warn(s"No executor for: $other")
    }
    info("Execution finished.")

class ProcessorState:
  var acc: List[Int] = Nil
  val mem: Array[List[Int]] = Array.fill(MEMORY_SIZE)(List.fill(REGISTER_WIDTH)(-1))

