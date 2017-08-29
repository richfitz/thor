##' Proxy object.  These exist to try and exploit LMDB's copy-free
##' design.  LMDB can pass back a read-only pointer to memory without
##' copying it.  So rather than immediately trying to read the whole
##' thing in, this class provides a "proxy" to the data.  At the
##' moment they're not terribly useful - all you can do is get the
##' length!  They are used internally in the package to support
##' cursors.
##'
##' @template mdb_val_proxy
##' @title Proxy values
##' @rdname mdb_proxy
##' @aliases mdb_proxy
##' @name mdb_proxy
##' @examples
##' # Start with a write transaction that has written a little data:
##' env <- thor::mdb_env(tempfile())
##' txn <- env$begin(write = TRUE)
##' txn$put("a", "apple")
##' txn$put("b", "banana")
##'
##' # We can get a proxy object back by passing as_proxy = TRUE
##' p <- txn$get("a", as_proxy = TRUE)
##' p
##'
##' # Without copying anything we can get the length of the data
##' p$size() # == nchar("apple")
##'
##' # And of course we can get the data
##' p$data()
##' p$data(as_raw = TRUE)
##'
##' # Referencing an invalid proxy is an error, but you can use
##' # "is_valid()" check to see if it is valid
##' p$is_valid()
##'
##' txn$put("c", "cabbage")
##' p$is_valid()
##' try(p$data())
NULL

mdb_val_proxy <- function(txn, ptr) {
  is_missing <- is.null(ptr)

  ## NOTE: a zero size for midding key makes sense because a
  ## zero-length data is not allowed (I believe).
  size <- if (is_missing) 0L else attr(ptr, "size", TRUE)

  mutations <- txn$.mutations
  assert_valid <- function() {
    if (!identical(txn$.mutations, mutations)) {
      reason <- if (is.null(txn$.ptr)) "been closed" else "modified database"
      stop("mdb_val_proxy is invalid: transaction has ", reason)
    }
  }

  ret <- list(
    is_valid = function() {
      identical(txn$.mutations, mutations)
    },
    size = function() {
      assert_valid()
      size
    },
    data = function(as_raw = NULL) {
      assert_valid()
      if (is_missing) {
        NULL
      } else {
        mdb_proxy_copy(ptr, as_raw)
      }
    })
  class(ret) <- "mdb_val_proxy"
  ret
}

##' @export
print.mdb_val_proxy <- function(x, ...) {
  cat(paste0(format_thor(x), "\n"))
}
