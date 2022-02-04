package g

import scala.util.matching.Regex

sealed trait Command

val MATCH_REGISTER = raw"(\d{$REGISTER_WIDTH})"
val MATCH_ADDRESS = raw"(\d+)"

object LOAD_CONST { val matcher: Regex = s"LOAD $MATCH_REGISTER".r }
case class LOAD_CONST(const: Seq[Int]) extends Command:
  override def toString: String = s"LOAD ${const.mkString}"

object SAVE_CONST_TO_ADDRESS { val matcher: Regex = s"SAVE $MATCH_REGISTER TO $MATCH_ADDRESS".r }
case class SAVE_CONST_TO_ADDRESS(const: Seq[Int], address: Int) extends Command:
  override def toString: String = s"SAVE ${const.mkString} TO $address"
