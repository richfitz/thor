## These are our flag holders; they will need exporting as data
## objects but I never remember how to do that!
flags_env <- new.env(parent = emptyenv())
flags_dbi <- new.env(parent = emptyenv())
flags_write <- new.env(parent = emptyenv())
flags_copy <- new.env(parent = emptyenv())
cursor_op <- new.env(parent = emptyenv())
NO_FLAGS <- NULL

init_flags <- function() {
  make_flag <- function(name, value, group) {
    structure(value, names = name, group = group, class = "mdb_flag")
  }
  init <- function(sym, group, e) {
    x <- .Call(sym)
    for (i in names(x)) {
      e[[i]] <- make_flag(i, x[[i]], group)
    }
    lockEnvironment(e)
    class(e) <- "mdb_flags"
  }
  init(Cmdb_flags_env,   "env",   flags_env)
  init(Cmdb_flags_dbi,   "dbi",   flags_dbi)
  init(Cmdb_flags_write, "write", flags_write)
  init(Cmdb_flags_copy,  "copy",  flags_copy)

  init(Cmdb_cursor_op, "cursor_op", cursor_op)
}

##' @export
print.mdb_flags <- function(x, ...) {
  cat(sprintf("<mdb_flags[%s]>\n", attr(x, "group")))
  cat(sprintf("  - %s\n", ls(x)), sep = "")
}

##' @export
print.mdb_flag <- function(x, ...) {
  cat(sprintf("<mdb_flag[%s]> %s\n",
              attr(x, "group"),
              paste(sprintf("%s (0x%x)", names(x), x), collapse = " | ")))
}

##' @export
c.mdb_flag <- function(...) {
  flags <- list(...)
  flags <- flags[!vlapply(flags, is.null)]
  if (length(flags) == 0) {
    return(NULL)
  }
  err <- !vlapply(flags, inherits, "mdb_flag")
  if (any(err)) {
    stop("Can only combine mdb_flag objects")
  }
  group <- vcapply(flags, attr, "group", exact = TRUE)
  if (length(unique(group)) != 1L) {
    stop("Expected a single group")
  }
  res <- NextMethod("c")
  class(res) <- "mdb_flag"
  attr(res, "group") <- group[[1L]]
  res
}
