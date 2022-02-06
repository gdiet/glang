# G Language

### Register Width

The REGISTER_WIDTH default is 16 decimal digits. Set environment variable G_REGISTER_WIDTH to change it.

### Script Syntax

* Empty lines are ignored.
* Lines starting with `// ` are ignored.
* One command per line.
* Commands and other keywords are UPPER CASE.
* User defined names are lower case including `_`.
* $REGISTER_WIDTH can be used.

### Commands

* `LOAD 0123456789012345` - load constant to accumulator
* `SAVE 0123456789012345 TO 1` - save constant to address
* 
