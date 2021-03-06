package g

import scala.util.chaining.scalaUtilChainingOps

object Parser extends ClassLogging:
  val commentMatcher: scala.util.matching.Regex = "//.*".r

  def parse(script: String): List[Command] =
    info("Starting parser...")
    script.linesIterator.toList.flatMap { line =>
      info(s"Parsing: $line")
      val tokens = line.split(' ').filterNot(_.isBlank).toList
      IS_COMMAND.unapply(tokens).tap(parseResult => info(s"=======> ${parseResult.getOrElse("")}"))
    }.tap(_ => info("Parsing finished."))

sealed trait Command

object IS_COMMAND:
  def unapply(tokens: List[String]): Option[Command] =
    tokens match
      case IS_LOAD(c) => Some(c)
      case IS_PRINT(c) => Some(c)
      case IS_SAVE(c) => Some(c)
      case IS_ADD(c) => Some(c)
      case IS_CLEAR(c) => Some(c)
      case IS_SET(c) => Some(c)
      case IS_IF(c) => Some(c)
      case IS_DEF(c) => Some(c)
      case IS_MARKER(c) => Some(c)
      case IS_GOTO(c) => Some(c)
      case IS_NOP(c) => Some(c) // Must be last as long as it has no fall-through.
      case other => fail(s"Don't understand tokens $tokens")

case class MARKER(name: String, command: Command) extends Command
object IS_MARKER:
  def unapply(tokens: List[String]): Option[MARKER] =
    tokens match
      case Is_Name(name) :: IS_COMMAND(command) => Some(MARKER(name, command))
      case other => None

case class GOTO(marker: String) extends Command
object IS_GOTO:
  def unapply(tokens: List[String]): Option[GOTO] =
    tokens match
      case List("GOTO", Is_Name(name)) => Some(GOTO(name))
      case other => None

case class IFCURRY(command: Command) extends Command
case class IFNOTFLAG(command: Command) extends Command
object IS_IF:
  def unapply(tokens: List[String]): Option[IFCURRY | IFNOTFLAG] =
    tokens match
      case "IF" :: "CURRY" :: IS_COMMAND(command) => Some(IFCURRY(command))
      case "IF" :: "NOT" :: "FLAG" :: IS_COMMAND(command) => Some(IFNOTFLAG(command))
      case other => None

case class NOP(tokens: List[String]) extends Command
object IS_NOP:
  def unapply(tokens: List[String]): Option[NOP] =
    tokens match
      case Nil | List("//") => Some(NOP(tokens))
      case other => Some(NOP(tokens)) // FIXME replace by None

case class LOAD(constant: Constant) extends Command
object IS_LOAD:
  def unapply(tokens: List[String]): Option[LOAD] =
    tokens match
      case List("LOAD", Is_Constant(constant)) => Some(LOAD(constant))
      case other => None

case class PRINTADDRESS(address: Address) extends Command
case object PRINTPROCESSOR extends Command
object IS_PRINT:
  def unapply(tokens: List[String]): Option[PRINTPROCESSOR.type | PRINTADDRESS] =
    tokens match
      case List("PRINT", "PROCESSOR") => Some(PRINTPROCESSOR)
      case List("PRINT", Is_Address(address)) => Some(PRINTADDRESS(address))
      case other => None

case object CLEARFLAG extends Command
object IS_CLEAR:
  def unapply(tokens: List[String]): Option[CLEARFLAG.type] =
    tokens match
      case List("CLEAR", "FLAG") => Some(CLEARFLAG)
      case other => None

case object SETFLAG extends Command
object IS_SET:
  def unapply(tokens: List[String]): Option[SETFLAG.type] =
    tokens match
      case List("SET", "FLAG") => Some(SETFLAG)
      case other => None

case class SAVECONST(constant: Constant, to: Address) extends Command
object IS_SAVE:
  def unapply(tokens: List[String]): Option[SAVECONST] =
    tokens match
      case List("SAVE", Is_Constant(constant), "TO", Is_Address(address)) =>
        Some(SAVECONST(constant, address))
      case other => None

case class ADDADDRESS(address: Address) extends Command
case object ADDCURRY1 extends Command
object IS_ADD:
  def unapply(tokens: List[String]): Option[ADDADDRESS | ADDCURRY1.type] =
    tokens match
      case List("ADD", Is_Address(address)) => Some(ADDADDRESS(address))
      case List("ADD", "CURRY", "RIGHT") => Some(ADDCURRY1)
      case other => None

case class DEF(name: String, params: List[NamedAddress]) extends Command
object IS_DEF:
  def unapply(tokens: List[String]): Option[DEF] =
    tokens match
      case "DEF" :: Is_Name(name) :: Is_DefParams(params) =>
        Some(DEF(name, params))
      case other => None

object Is_DefParams:
  def unapply(tokens: List[String]): Option[List[NamedAddress]] =
    tokens match
      case Is_Name(name) :: "ADDRESS" :: Is_DefParams(params) => Some(NamedAddress(name) :: params)
      case other => None

case class Address(position: Int)
case class NamedAddress(name: String)
case class Constant(value: List[Int]) { override def toString: String = value.mkString }

object Is_Constant:
  def unapply(string: String): Option[Constant] =
    if string.matches(s"[0-9]{$REGISTER_WIDTH}")
    then Some(Constant(string.getBytes.map(_ - '0').toList)) else None

object Is_Address:
  def unapply(string: String): Option[Address] =
    if string.matches(s"[0-9]+")
    then Some(Address(string.toInt)) else None

object Is_Name:
  def unapply(string: String): Option[String] =
    if string.matches("[a-z_]+") then Some(string) else None

