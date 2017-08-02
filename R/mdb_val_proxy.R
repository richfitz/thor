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
