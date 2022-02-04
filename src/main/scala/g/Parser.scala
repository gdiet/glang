package g

import scala.util.chaining.scalaUtilChainingOps

object Parser extends ClassLogging:
  val commentMatcher: scala.util.matching.Regex = "//.*".r

  def parse(script: String): Vector[Command] =
    script.linesIterator.toVector.flatMap { line =>
      info(s"Parsing: $line")
      parseLine(line).tap(parseResult => info(s"=======> ${parseResult.getOrElse("")}"))
    }

  def parseLine(line: String): Option[Command] =
    line match
      case LOAD_CONST.matcher(const) =>
        Some(LOAD_CONST(const.map(_-'0')))
      case SAVE_CONST_TO_ADDRESS.matcher(const, address) =>
        Some(SAVE_CONST_TO_ADDRESS(const.map(_-'0'), address.toInt))
      case DEF.matcher(name, args) =>
        Some(DEF(name, args.split(" ").toList))
      case ADD_REFERENCE.matcher(reference) =>
        Some(ADD_REFERENCE(reference))

      case "" | commentMatcher() => None
      case other => fail(s"Don't understand line [$line]")
