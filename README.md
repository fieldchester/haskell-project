# Coordinating dependent threads

The threads communicate through writing and reading variables in non locking manner.

The coordinating of the reading and writing happens so that the dependendy among the threads is respected.

The type of this variables is monadic and the chainig reflects the dependency.

A way to implement this is to have at least one queue for each of those variables.

