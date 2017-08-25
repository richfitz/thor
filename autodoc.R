#!/usr/bin/env Rscript

## Dirty hack to compile docs in the absence of proper Roxygen R6 support.
devtools::load_all(".")

add_usage <- function(dat, object) {
  capture_usage <- function(name) {
    tmp <- capture.output(args(object$public_methods[[name]]))
    tmp <- strip_trailing_whitespace(paste(tmp[-length(tmp)], collapse="\n"))
    sub("^function\\s*", name, tmp)
  }

  valid <- names(object$public_methods)
  extra <- setdiff(names(dat), valid)
  if (length(extra) > 0L) {
    warning(sprintf("In '%s', extra methods: %s",
                    object$classname,
                    paste(extra, collapse=", ")),
            immediate.=TRUE, call.=FALSE)
  }


  groups <- object$public_fields$.methods
  if (!is.null(groups)) {
    m <- unlist(groups, use.names = FALSE)
    msg <- setdiff(m, names(dat))
    if (length(msg) > 0L) {
      warning(sprintf("%d missing methods for %s:\n  %s",
                      length(msg), object$classname,
                      paste(msg, collapse = ", ")),
              immediate. = TRUE)
    }
    dat <- dat[order(match(names(dat), m))]
  }

  for (name in names(dat)) {
    dat[[name]]$method_name <- name
    dat[[name]]$usage <- capture_usage(name)
    dat[[name]]$order <- names(formals(object$public_methods[[name]]))
  }
  dat
}

indent <- function(str, n, pad=NULL) {
  if (is.null(pad)) {
    pad <- paste(rep(" ", n), collapse="")
  }
  p <- function(s) {
    paste(paste0(pad, s), collapse="\n")
  }
  vapply(strsplit(str, "\n"), p, character(1))
}

format_params <- function(xp) {
  fmt1 <- "\\itemize{\n%s\n}"
  fmt2 <- "\\item{\\code{%s}: %s\n}\n"
  pars <- sprintf(fmt2, names(xp), indent(unlist(xp), 2))
  sprintf(fmt1, indent(paste(pars, collapse="\n"), 2))
}

format_method <- function(x) {
  title <- sprintf("\\item{\\code{%s}}{", x$method_name)
  end <- "}"

  p_msg   <- setdiff(x$order, names(x$params))
  p_extra <- setdiff(names(x$params), x$order)
  if (length(p_msg) > 0) {
    warning(sprintf("In '%s', missing parameters: %s",
                    x$method_name, paste(p_msg, collapse=", ")),
            immediate.=TRUE, call.=FALSE)
  }
  if (length(p_extra) > 0) {
    warning(sprintf("In '%s', extra parameters: %s",
                    x$method_name, paste(p_extra, collapse=", ")),
            immediate.=TRUE, call.=FALSE)
  }
  ## preseve order, though I'm pretty sure that the yaml package is
  ## actually preserving it.
  if (length(p_msg) == 0 && length(p_extra) == 0) {
    x$params <- x$params[x$order]
  }

  body <- sprintf("%s\n\n\\emph{Usage:}\n\\code{%s}",
                  x$short, x$usage)
  if (!is.null(x$params)) {
    body <- paste0(body, "\n\n\\emph{Arguments:}\n", format_params(x$params))
  }
  if (!is.null(x$details)) {
    body <- paste0(body, "\n\n\\emph{Details:}\n", x$details)
  }
  if (!is.null(x$value)) {
    body <- paste0(body, "\n\n\\emph{Value}:\n", x$value)
  }
  if (!is.null(x$mdb)) {
    ref <- collapse_str(sprintf("\\code{%s()}", x$mdb))
    body <- paste0(body, "\n\n\\emph{Note}: In lmdb.h this is ", ref)
  }
  paste(title, indent(body, 2), end, sep="\n")
}

strip_trailing_whitespace <- function(x) {
  gsub("[ \t]+(\n|$)", "\\1", x)
}

format_class <- function(x) {
  ret <- vapply(x, format_method, character(1))
  ret <- sprintf("@section Methods:\n\n\\describe{\n%s\n}",
                 paste(ret, collapse="\n"))
  ret <- indent(ret, pad="##' ")
  strip_trailing_whitespace(ret)
}

## From remake, rrqueue, etc, etc.
yaml_load <- function(string) {
  handlers <- list(`bool#yes` = function(x) {
    if (identical(toupper(x), "TRUE")) TRUE else x
  }, `bool#no` = function(x) {
    if (identical(toupper(x), "FALSE")) FALSE else x
  })
  yaml::yaml.load(string, handlers = handlers)
}
yaml_read <- function(filename) {
  yaml_load(paste(readLines(filename), collapse="\n"))
}

process <- function() {
  files <- dir("man-roxygen", full.names = TRUE, pattern = "\\.yml$")
  files <- files[!grepl("helper\\.yml$", files)]
  for (file in files) {
    process1(file)
  }
}

process1 <- function(filename) {
  object <- get(paste0("R6_", sub("\\.yml", "", basename(filename))))
  dat <- yaml_read(filename)
  if (object$classname == "mdb_env") {
    extra <- yaml_read(file.path(dirname(filename), "mdb_txn.yml"))
    take <- list(ro = c("get", "exists", "list", "mget"),
                 rw = c("put", "del", "mput", "mdel"))
    info <- yaml_read(file.path(dirname(filename), "mdb_env_helper.yml"))
    for (v in names(take)) {
      for (i in take[[v]]) {
        x <- extra[[i]]
        x$params$as_proxy <- NULL
        x$params$db <- info$db
        if (is.null(x$details)) {
          x$details <- info[[v]]
        } else {
          x$details <- paste(x$details, info[[v]], sep = "\n\n")
        }
        dat[[i]] <- x
      }
    }
  }
  dat <- add_usage(dat, object)
  str <- format_class(dat)
  dest <- sub("\\.yml", ".R", filename)
  message("writing ", dest)
  writeLines(str, dest)
}

collapse_str <- function(x) {
  n <- length(x)
  if (n == 1L) {
    x
  } else if (n == 2L) {
    paste0(x[[1]], " & ", x[[2]])
  } else if (n > 2L) {
    paste0(paste(x[-n], collapse = ", "), " & ", x[[n]])
  }
}

if (!interactive() && identical(commandArgs(TRUE), "process")) {
  process()
}
