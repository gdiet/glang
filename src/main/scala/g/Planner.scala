package g

object Planner extends ClassLogging:

  def plan(commands: List[Command]): Unit =
    (0 until REGISTER_WIDTH).foreach { actor =>
      info("")
      info(s"Plan für Aktor $actor:")
      commands.foreach {
        case NOP(_) => /**/
        case command => info(plan(command, actor))
      }
    }

  def plan(command: Command, actor: Int): String =
    command match
      case LOAD(constant) => s"Setze A auf ${constant.value(actor)}"
      case SAVECONST(constant, to) => s"Schreibe ${constant.value(actor)} in Speicher ${to.position}"
      case ADDADDRESS(address) => s"Addiere Speicher ${address.position} zu A, Überlauf in C"
      case MARKER(marker, subcommand) => s"Sprungmarke: $marker - ${plan(subcommand, actor)}"
      case CLEARFLAG => if actor == 0 then "Lösche Flag F" else "Einmal aussetzen"
      case IFCURRY(command) => ???
      case other => s"NICHT IMPLEMENTIERT: $command"
