package g

import scala.util.matching.Regex

sealed trait Command

val MATCH_REGISTER = raw"(\d{$REGISTER_WIDTH})"
val MATCH_ADDRESS = raw"(\d+)"
val MATCH_NAME = raw"([a-z_]+)"

object LOAD_CONST { val matcher: Regex = s"LOAD $MATCH_REGISTER".r }
case class LOAD_CONST(const: Seq[Int]) extends Command:
  override def toString: String = s"LOAD ${const.mkString}"

object SAVE_CONST_TO_ADDRESS { val matcher: Regex = s"SAVE $MATCH_REGISTER TO $MATCH_ADDRESS".r }
case class SAVE_CONST_TO_ADDRESS(const: Seq[Int], address: Int) extends Command:
  override def toString: String = s"SAVE ${const.mkString} TO $address"

object DEF { val matcher: Regex = s"DEF $MATCH_NAME (.+)".r }
case class DEF(name: String, args: List[String]) extends Command:
  override def toString: String = s"DEF $name ${args.mkString(" ")}"

object ADD_REFERENCE { val matcher: Regex = s"ADD $MATCH_NAME".r }
case class ADD_REFERENCE(reference: String) extends Command:
  override def toString: String = s"ADD $reference"
