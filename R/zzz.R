##' @useDynLib thor, .registration = TRUE
##' @importFrom R6 R6Class
NULL


## enum dict of cursor operations
cursor_op <- new.env(parent = emptyenv())


## Global cache of write transactions used by the environment
write_txns <- new.env(parent = emptyenv())


.onLoad <- function(...) {
  list_to_fixed_env(.Call(Cmdb_cursor_op), cursor_op) # nocov
}
