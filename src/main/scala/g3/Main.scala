package g3

import scala.util.Using.resource

@main def main(): Unit =
  val script = """
FUNCTION add_unsigned: $x $y $z
LOAD $x TO A
ADD  $y TO A,B
CLEAR F
loop: CLEAR G
0: IF B > 0 THEN SET F; 1..N: IF B > 0 THEN SET G
IF NOT G THEN RETURN
SHIFTLEFT B
IF B > 0 THEN ADD #1 TO A,B
GOTO loop

SAVE #0678 TO $0
SAVE #9876 TO $1
CALL add_unsigned $0 $1 $2
// END
"""
  val lines = script.linesIterator.toVector
  resource(Manager(Settings(), lines)) { manager =>

  }
