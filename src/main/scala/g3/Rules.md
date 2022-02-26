# Syntax

* Source code is processed by lines.
* The lines are stripped of comments "// .*" and then trimmed.
* User defined names must be like "[a-z_]+".
* "FUNCTION [name]:.*" with optional parameters defines a function.
* Function definitions must be terminated by a matching "END_FUNCTION".
* Function definitions may not be nested.
* Function parameters are space separated.
* When a function is called, the function parameters define the context for subsequent commands
* The function context ends and the execution pointer returns when "RETURN" is executed.
* "[name]:.*" defines a GOTO target with optional command appended.
* Only the commands below can be appended to a GOTO target.

# Execution

* Pass 1: Initialize table with GOTO targets and FUNCTION definitions.
