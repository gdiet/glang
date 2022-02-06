package g

object Executor extends ClassLogging:

  def run(commands: List[Command]): Unit =
    run(ProcessorState(), commands)

  def run(state: ProcessorState, commands: List[Command]): Unit =
    info("Starting execution...")
    runAtPosition(state, commands, 0)
    info("Execution finished.")

  @annotation.tailrec
  def runAtPosition(state: ProcessorState, commands: List[Command], position: Int): Unit =
    if commands.size > position then
      val command = commands(position)
      info(s"$command")
      val nextPosition = runCommand(state, commands, commands(position), position)
      runAtPosition(state, commands, nextPosition)

  @annotation.tailrec
  def runCommand(state: ProcessorState, commands: List[Command], command: Command, position: Int): Int =
    command match
      // Commands that might change the position arbitrarily.
      case IFCURRY(conditional) =>
        if state.curry.contains(true) then runCommand(state, commands, conditional, position) else position + 1
      case MARKER(name, command) => runCommand(state, commands, command, position)
      case GOTO(marker) => commands.indexWhere { case MARKER(`marker`, _) => true; case other => false }
      case other =>
        command match
          // Commands that advance the position by 1:
          case NOP(_) => /**/
          case LOAD(constant) => state.acc = constant.value
          case PRINTPROCESSOR =>
            info(s"Flag       : ${state.flag}")
            info(s"Accumulator: ${state.acc.mkString}")
            info(s"Curry flags: ${state.curry.map(c => if c then "*" else " ").mkString}")
          case PRINTADDRESS(address) => info(s"Memory at ${address.position} is ${state.mem(address.position).mkString.replaceAll("-\\d", "#")}")
          case SAVECONST(constant, to) => state.mem(to.position) = constant.value
          case ADDADDRESS(address) =>
            val (curry, acc) = state.acc.zip(state.mem(address.position))
              .map { case (a,b) => (a+b > 9, (a+b) % 10) }.unzip
            state.curry = curry; state.acc = acc
          case CLEARFLAG => state.flag = false
          case SETFLAG => state.flag = true
          case other => warn(s"No executor for: $other")
        position + 1

class ProcessorState:
  var flag: Boolean = false
  var acc: List[Int] = Nil
  var curry: List[Boolean] = Nil
  val mem: Array[List[Int]] = Array.fill(MEMORY_SIZE)(List.fill(REGISTER_WIDTH)(-1))
