##' Proxy object.  These exist to try and exploit LMDB's copy-free
##' design.  LMDB can pass back a read-only pointer to memory without
##' copying it.  So rather than immediately trying to read the whole
##' thing in, this class provides a "proxy" to the data.  At the
##' moment they're not terribly useful - all you can do is get the
##' length, and peek at the first bytes!  They are used internally in
##' the package to support cursors.
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
##'
##' # It is possible to read the first few bytes; this might be useful
##' # to determine if (say) a value is a serialised R object:
##' txn$put("d", serialize(mtcars, NULL))
##'
##' # The first 6 bytes of a binary serialised rds object is always
##' #
##' #   0x58 0x0a 0x00 0x00 0x00 0x02
##' #
##' # for XDR serialisation, or
##' #
##' #   0x42 0x0a 0x02 0x00 0x00 0x00
##' #
##' # for native little-endian serialisation.
##' #
##' # So with a little helper function
##' is_rds <- function(x) {
##'   h_xdr <- as.raw(c(0x58, 0x0a, 0x00, 0x00, 0x00, 0x02))
##'   h_bin <- as.raw(c(0x42, 0x0a, 0x02, 0x00, 0x00, 0x00))
##'   x6 <- head(x, 6L)
##'   identical(x6, h_xdr) || identical(x6, h_bin)
##' }
##'
##' # We can see that the value stored at 'a' is not rds
##' p1 <- txn$get("a", as_proxy = TRUE)
##' is_rds(p1$head(6, as_raw = TRUE))
##'
##' # But the value stored at 'd' is:
##' p2 <- txn$get("d", as_proxy = TRUE)
##' is_rds(p2$head(6, as_raw = TRUE))
##'
##' # Retrieve and unserialise the value:
##' head(unserialize(p2$data()))
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
    },

    head = function(n = 6L, as_raw = NULL) {
      assert_valid()
      if (is_missing) {
        NULL
      } else {
        mdb_proxy_head(ptr, n, as_raw)
      }
    },

    is_raw = function() {
      mdb_proxy_is_raw(ptr)
    })

  class(ret) <- "mdb_val_proxy"
  ret
}

##' @export
print.mdb_val_proxy <- function(x, ...) {
  cat(paste0(format_thor(x), "\n"))
}
