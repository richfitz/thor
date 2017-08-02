##' @useDynLib thor, .registration = TRUE
cursor_op <- new.env(parent = emptyenv())
.onLoad <- function(...) {
  list_to_fixed_env(.Call(Cmdb_cursor_op), cursor_op) # nocov
}

## Global cache of write transactions used by the environment
write_txns <- new.env(parent = emptyenv())
