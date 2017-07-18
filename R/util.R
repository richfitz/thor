vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}
viapply <- function(X, FUN, ...) {
  vapply(X, FUN, integer(1), ...)
}
vnapply <- function(X, FUN, ...) {
  vapply(X, FUN, numeric(1), ...)
}
vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

is_null_pointer <- function(x) {
  .Call(Cis_null_pointer, x)
}

## A really simple little stack thing.  This is going to hold
## references that can be compared with `identical`; because we pass
## everything by reference this should all work reasonably well.  We
## might want to swap this out for using a pairlist.  Alternatively,
## It might be nicer to keep a size and avoid allocations but let's
## assume that R is doing that for us, because it avoids a lot of book
## keeping to do so!  It's not that bad though; we'd pre-allocate 10
## element vector, keep an index.  Removals would set NULL and
## decrease index, or we could compact on resize.  Another way of
## solving this would be keep things in an actual hash but that
## requires getting a unique identifier for each environment.  We
## might be able to get away with using the environment address I
## guess.
stack <- function() {
  data <- list()

  list(
    clear = function() {
      data <<- list()
    },
    ## This adds to the set only if its a new thing.
    add = function(x) {
      for (i in seq_along(data)) {
        if (identical(data[[i]], x)) {
          return()
        }
      }
      data[[length(data) + 1L]] <<- x
    },
    push = function(x) {
      data[[length(data) + 1L]] <<- x
    },
    pop = function(empty = NULL) {
      n <- length(data)
      if (n == 0L) {
        return(empty)
      }
      ret <- data[[n]]
      data[[n]] <<- NULL
      ret
    },
    get = function(x) {
      data
    },
    length = function() {
      length(data)
    },
    discard = function(x) {
      for (i in seq_along(data)) {
        if (identical(data[[i]], x)) {
          data[[i]] <<- NULL
          break
        }
      }
    })
}

assert_is <- function(x, what, name = deparse(substitute(x))) {
  if (!inherits(x, what)) {
    stop(sprintf("'%s' must be a %s", name,
                 paste(what, collapse = " / ")), call. = FALSE)
  }
}
