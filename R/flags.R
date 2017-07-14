## These are our flag holders; they will need exporting as data
## objects but I never remember how to do that!
flags_env <- new.env(parent = emptyenv())
flags_put <- new.env(parent = emptyenv())
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
  init(Cmdb_flags_env, "env", flags_env)
  init(Cmdb_flags_put, "put", flags_put)

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
`|.mdb_flag` <- function(e1, e2) {
  ## TODO: consider treating logical(0) as a NULL flag because that's
  ## what NULL | NULL evaluates to.  c(...) would be nicer but does
  ## not work unless the *first* argument is the flag.
  if (missing(e2) || is.null(e2)) {
    ## Unary +e1
    return(e1)
  }
  if (is.null(e1)) {
    return(e2)
  }
  if (!inherits(e1, "mdb_flag") || !inherits(e2, "mdb_flag")) {
    stop("Can only combine mdb_flag objects")
  }

  group_name <- attr(e1, "group_name")
  if (group_name == "cursor_op") {
    ## This is a bit nasty because we let cursor_op | NULL (and v.v.) through
    stop("Can't combine cursor_op flags")
  }

  group_id <- attr(e1, "group_id")
  if (!identical(group_id, attr(e2, "group_id"))) {
    stop("Expected a single group type")
  }

  keep <- !e2 %in% e1
  if (any(keep)) {
    res <- c(e1, e2[!e2 %in% e1])
    class(res) <- "mdb_flag"
    attr(res, "group_id") <- group_id
    attr(res, "group_name") <- group_name
  } else {
    res <- e1
  }
  res
}

get_flag <- function(e, name) {
  .subset2(e, name) %||%
    stop(sprintf("Unknown flag '%s'", name), call. = FALSE)
}

##' @export
`$.mdb_flags` <- function(x, name) {
  get_flag(x, name)
}

##' @export
`[[.mdb_flags` <- function(x, name) {
  get_flag(x, name)
}

as_mdb_flag <- function(x, flags) {
  value <- viapply(names(flags), get, flags)
  used <- names(value)[bitwAnd(x, value) != 0]
  res <- NULL
  for (u in used) {
    res <- res | flags[[u]]
  }
  res
}
