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
      case ADDADDRESS(address) => s"Addiere Speicher ${address.position} zu Akkumulator A, setze/lösche Überlauf C"
      case MARKER(marker, subcommand) => s"Sprungmarke: $marker - ${plan(subcommand, actor)}"
      case CLEARFLAG => if actor == 0 then "Lösche Prozessor-Flag F" else "Einmal aussetzen"
      case IFCURRY(subcommand) => s"Wenn Überlauf C gesetzt ist, dann: ${plan(subcommand, actor)}"
      case SETFLAG => "Setze Prozessor-Flag F"
      case IFNOTFLAG(subcommand) => s"Wenn Prozessor-Flag F nicht gesetzt ist, dann: ${plan(subcommand, actor)}"
      case ADDCURRY1 =>
        if actor == REGISTER_WIDTH - 1 then "Lösche Überlauf C"
        else "Addiere 1 zu Akkumulator A, setze/lösche Überlauf C"
      case GOTO(marker) => s"Gehe zu Sprungmarke: $marker"
      case PRINTPROCESSOR =>
        if actor == 0 then "Veröffentliche Prozessor-Flag F, Akkumulator-Wert A und Überlauf C"
        else "Veröffentliche Akkumulator-Wert A und Überlauf C"
      case other => s"NICHT IMPLEMENTIERT: $command"
