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

as_integer <- function(x, name = deparse(substitute(x))) {
  if (is.integer(x)) {
    x
  } else {
    if (length(x) != 1L) {
      ## protect from the if below because we only use this for
      ## scalars.  The check for the previous clause is done in the C
      ## handling code.
      stop(sprintf("'%s' must be a scalar", name))
    }
    ret <- as.integer(x)
    if (ret != x) {
      stop(sprintf("'%s' must be an integer, or integer-like", name),
           call. = FALSE)
    }
    ret
  }
}

list_to_fixed_env <- function(x, env) {
  for (i in names(x)) {
    env[[i]] <- x[[i]]
    lockBinding(i, env)
  }
  lockEnvironment(env)
  invisible(env)
}

capture_args <- function(f, name) {
  args <- capture.output(args(f))
  sub("function ", name,
      trimws(paste(args[-length(args)], collapse = "\n")))
}
