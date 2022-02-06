package g

import scala.util.chaining.scalaUtilChainingOps

object Parser extends ClassLogging:
  val commentMatcher: scala.util.matching.Regex = "//.*".r

  def parse(script: String): Vector[Command] =
    script.linesIterator.toVector.flatMap { line =>
      info(s"Parsing: $line")
      val tokens = line.split(' ').filterNot(_.isBlank).toList
      val replace = Map("$REGISTER_WIDTH" -> s"$REGISTER_WIDTH")
      parse(replace, tokens).tap(parseResult => info(s"=======> ${parseResult.getOrElse("")}"))
    }

  def parse(replace: Map[String, String], tokens: List[String]): Option[Command] =
    tokens match
      case DEF(c) => Some(c)
      case LOAD(c) => Some(c)
      case NOP(c) => Some(c) // Must be last as long as it has no fall-through.
      case other => fail(s"Don't understand tokens $tokens")

sealed trait Command

case class NOP(tokens: List[String]) extends Command
object NOP:
  def unapply(tokens: List[String]): Option[NOP] =
    tokens match
      case Nil | List("//") => Some(NOP(tokens))
      case other => Some(NOP(tokens)) // FIXME replace by None

case class LOAD(constant: Constant) extends Command
object LOAD:
  def unapply(tokens: List[String]): Option[LOAD] =
    tokens match
      case List("LOAD", Constant(constant)) => Some(LOAD(constant))
      case other => None

case class DEF(name: String, params: List[NamedAddress]) extends Command
object DEF:
  def unapply(tokens: List[String]): Option[DEF] =
    tokens match
      case "DEF" :: Name(name) :: DefParams(params) =>
        Some(DEF(name, params))
      case other => None

object DefParams:
  def unapply(tokens: List[String]): Option[List[NamedAddress]] =
    tokens match
      case Name(name) :: "ADDRESS" :: DefParams(params) => Some(NamedAddress(name) :: params)
      case other => None

case class NamedAddress(name: String)
case class Constant(value: String)

object Constant:
  def unapply(string: String): Option[Constant] =
    if string.matches(s"[0-9]{$REGISTER_WIDTH}") then Some(Constant(string)) else None

object Name:
  def unapply(string: String): Option[String] =
    if string.matches("[a-z_]+") then Some(string) else None

