# Syntax

* Source code is processed by lines.
* The lines are stripped of comments "// .*" and then trimmed.
* User defined names must be like "[a-z_]+".
* "^[name]:.*" defines a GOTO target.
* "^FUNCTION [name]:.*" (with optional parameters) defines a function.

# Execution

* Pass 1: Initialize table with GOTO targets and FUNCTION definitions.
