## These are our flag holders; they will need exporting as data
## objects but I never remember how to do that!
flags_env <- new.env(parent = emptyenv())
flags_dbi <- new.env(parent = emptyenv())
flags_txn <- new.env(parent = emptyenv())
flags_write <- new.env(parent = emptyenv())
cursor_op <- new.env(parent = emptyenv())
NO_FLAGS <- NULL

init_flags <- function() {
  make_flag <- function(name, value, group_name, group_id) {
    structure(value, names = name,
              group_name = group_name,
              group_id = group_id,
              class = "mdb_flag")
  }
  init <- function(sym, group_name, e) {
    x <- .Call(sym)
    group_id <- attr(x, "group_id")
    for (i in names(x)) {
      e[[i]] <- make_flag(i, x[[i]], group_name, group_id)
    }
    lockEnvironment(e)
    class(e) <- "mdb_flags"
  }
  init(Cmdb_flags_env,   "env",   flags_env)
  init(Cmdb_flags_dbi,   "dbi",   flags_dbi)
  init(Cmdb_flags_txn,   "txn",   flags_txn)
  init(Cmdb_flags_write, "write", flags_write)

  init(Cmdb_cursor_op, "cursor_op", cursor_op)
}

##' @export
print.mdb_flags <- function(x, ...) {
  cat(sprintf("<mdb_flags[%s]>\n", attr(x, "group_name")))
  cat(sprintf("  - %s\n", ls(x)), sep = "")
}

##' @export
print.mdb_flag <- function(x, ...) {
  cat(sprintf("<mdb_flag[%s]> %s\n",
              attr(x, "group_name"),
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
  if (length(flags) == 1L) {
    return(flags[[1L]])
  }

  group_name <- attr(flags[[1L]], "group_name")
  if (group_name == "cursor_op") {
    stop("Can't combine cursor_op flags")
  }

  group_id <- viapply(flags, attr, "group_id", exact = TRUE)
  if (length(unique(group_id)) != 1L) {
    stop("Expected a single group type")
  }

  res <- NextMethod("c")
  class(res) <- "mdb_flag"
  attr(res, "group_id") <- group_id[[1L]]
  attr(res, "group_name") <- group_name
  res
}

get_flag <- function(e, name) {
  .subset2(e, name) %||%
    stop(sprintf("Unknown flag %s", name), call. = FALSE)
}

##' @export
`$.mdb_flags` <- function(x, name) {
  get_flag(x, name)
}

##' @export
`[[.mdb_flags` <- function(x, name) {
  get_flag(x, name)
}
