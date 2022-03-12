/*

COPY $5 TO §A
COPY $x TO §A

ADD $5 TO §A,§B
ADD $x TO §A,§B

NOP

SET !F TRUE
SET !G FALSE

IF §B > #4 THEN CODE
IF §A < #4 THEN CODE
IF §C = #0 THEN CODE

IF !F THEN CODE
IF NOT !G THEN CODE

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
RETURN // TODO remove - just necessary as long as the rest is not yet complete
END_FUNCTION

SAVE #0678 TO $0
target_no_command:
example_target: SAVE #9876 TO $1
RETURN // TODO remove - just for first implementation
CALL add_unsigned $0 $1 $2
// END
"""
  val lines = script.linesIterator.toVector
  resource(Manager(lines)) { manager =>
    manager.execute(0)
  }
*/
