with_new_txn <- function(env, write, f, sync = NULL, metasync = NULL) {
  parent <- NULL
  txn_ptr <- env$.new_txn_ptr(write, parent, sync, metasync,
                              temporary = TRUE)
  if (write) {
    withCallingHandlers({
      ret <- f(txn_ptr)
      mdb_txn_commit(txn_ptr)
      ret
    }, error = function(e) mdb_txn_abort(txn_ptr, FALSE))
  } else {
    on.exit({
      mdb_txn_reset(txn_ptr)
      env$.spare_txns$push(txn_ptr)
    })
    f(txn_ptr)
  }
}

invalidate_dependencies <- function(x) {
  if (!is.null(x$.deps)) {
    deps <- x$.deps$get()
    for (d in rev(deps)) {
      d$.invalidate()
    }
    x$.deps <- NULL
  }
}

format_thor <- function(x) {
  exclude <- c("initialize", "finalize", "format")
  method_names <- setdiff(ls(x, pattern = "^[^.]"), exclude)
  methods <- vapply(method_names, function(i) capture_args(x[[i]], i),
                    character(1), USE.NAMES = FALSE)

  groups <- x$.methods %||% list(Public = method_names)
  methods2 <- vapply(groups, function(i)
    paste0("    ", methods[match(i, method_names)], collapse = "\n"),
    character(1), USE.NAMES = FALSE)
  paste(c(sprintf("<%s>", class(x)[[1]]),
          sprintf("  %s:\n%s", names(groups), methods2)),
        collapse = "\n")
}
