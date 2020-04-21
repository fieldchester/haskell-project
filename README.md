# Coordinating dependent threads

The threads communicate through writing and reading variables.

The coordinating of the reading and writing happens so that the dependendy among the threads is respected.

The type of this variables is monadic and the chainig reflects the dependency.

A way to implement this is to have at least one queue for each of those variables.

In a first variant writing to the variable is asynchron and reading is synchron.

Example without threading:
    input   output
init
var1    0   0
var2    0   0
var3    0   1

fetch
var1    1   0
var2    0   0
var3    0   0
prog
var1    0   2
var2    0   0
var3    0   0

fetch
var1    0   0
var2    2   0
var3    0   0
prog
var1    0   0
var2    0   3
var3    0   0

...

Example with threading:
    input   output
init
var1    0   1
var2    0   0
var3    0   1

