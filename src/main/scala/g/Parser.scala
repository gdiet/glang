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
      case Nil | List("//") => None
      case DEF(sub) => Some(sub)
      case other => fail(s"Don't understand tokens $tokens")

sealed trait Command

case class DEF(name: String) extends Command
object DEF:
  def unapply(tokens: List[String]): Option[DEF] =
    tokens match
      case List("DEF", Name(name)) => Some(DEF(name))
      case other => None

case class NamedAddress(name: String)

object DEF_PARAMS:
  def unapply(tokens: List[String]): Option[List[NamedAddress]] =
    tokens match
      case Name(name) :: DEF_PARAMS(params) => Some(NamedAddress(name) :: params)
      case other => None

object Name:
  def unapply(string: String): Option[String] =
    if string.matches("[a-z_]") then Some(string) else None

