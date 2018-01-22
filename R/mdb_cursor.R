##' Cursors are required for some advanced queries on an mdb database,
##' when the basic set of functions in \code{\link{mdb_txn}} is not
##' sufficient.
##'
##' Cursors must be created from within a transaction (which in turn
##' are created from an environment).
##'
##' @template mdb_cursor
##' @title Use mdb transactions
##' @rdname mdb_cursor
##' @aliases mdb_cursor
##' @name mdb_cursor
##' @examples
##' # Start by creating a new environment, and within that a write
##' # transaction, and from that a new cursor.  But first put a bunch
##' # of data into the database
##' env <- thor::mdb_env(tempfile())
##' env$mput(letters, LETTERS)
##' txn <- env$begin(write = TRUE)
##' cur <- txn$cursor()
##'
##' # Move the cursor to the first position
##' cur$first()
##'
##' # The key and value:
##' cur$key()
##' cur$value()
##'
##' # Move to a different key:
##' cur$move_to("g")
##' cur$value()
##'
##' # Delete the current item
##' cur$del()
##' cur$key()
##'
##' # We can't move to 'g' any more as it's gone:
##' (cur$move_to("g"))
##' cur$key() # NULL
##'
##' # But we can *seek* 'g', which will move to 'h'
##' (cur$seek("g"))
##' cur$key() # "h"
##'
##' # Get raw values out:
##' cur$value(as_raw = TRUE)
##'
##' # Cleanup
##' env$destroy()
NULL

R6_mdb_cursor <- R6::R6Class(
  "mdb_cursor",
  cloneable = FALSE,
  public = list(
    .txn = NULL,
    .db = NULL,
    .ptr = NULL,
    .cur_key = NULL,
    .cur_value = NULL,
    .valid = FALSE,
    .methods = list(
      Finish = "close",
      Actions = c("put", "del", "replace", "pop"),
      Movement = c("first", "last", "move_next", "move_prev"),
      Find = c("move_to", "seek", "get"),
      Current = c("is_valid", "key", "value"),
      Duplicates = c("first_dup", "last_dup",
                     "move_next_dup", "move_prev_dup",
                     "move_next_nodup", "move_prev_nodup",
                     "move_to_dup", "seek_dup", "count")),

    initialize = function(txn) {
      self$.txn <- txn
      self$.db <- txn$.db
      self$.ptr <- mdb_cursor_open(self$.txn$.ptr, self$.db$.ptr)
      txn$.deps$add(self)
    },

    finalize = function() {
      self$.invalidate()
    },

    format = function() {
      format_thor(self)
    },

    .invalidate = function() {
      if (!is.null(self$.ptr)) {
        self$close()
      }
    },

    close = function() {
      mdb_cursor_close(self$.ptr)
      ## self$.db$.deps$discard(self)
      self$.txn$.deps$discard(self)
      self$.txn <- NULL
      self$.db <- NULL
      self$.ptr <- NULL
    },

    .cursor_get = function(cursor_op, key = NULL, value = NULL) {
      x <- mdb_cursor_get(self$.ptr, cursor_op, key, value)
      self$.cur_key <- mdb_val_proxy(self$.txn, x[[1L]])
      self$.cur_value <- mdb_val_proxy(self$.txn, x[[2L]])
      self$.valid <- !is.null(x[[2L]])
    },

    is_valid = function() {
      self$.valid
    },

    ## TODO: pass value_if_missing through to value and make size NA for these?
    key = function(as_proxy = FALSE, as_raw = NULL) {
      if (is.null(self$.cur_key) || !self$.cur_key$is_valid()) {
        self$.cursor_get(cursor_op$GET_CURRENT)
      }
      if (as_proxy) {
        self$.cur_key
      } else {
        self$.cur_key$data(as_raw)
      }
    },

    value = function(as_proxy = FALSE, as_raw = NULL) {
      if (is.null(self$.cur_value) || !self$.cur_value$is_valid()) {
        self$.cursor_get(cursor_op$GET_CURRENT)
      }
      if (as_proxy) {
        self$.cur_value
      } else {
        self$.cur_value$data(as_raw)
      }
    },

    first = function() {
      self$.cursor_get(cursor_op$FIRST)
    },
    last = function() {
      self$.cursor_get(cursor_op$LAST)
    },
    move_prev = function() {
      self$.cursor_get(cursor_op$PREV)
    },
    move_next = function() {
      self$.cursor_get(cursor_op$NEXT)
    },

    move_to = function(key) {
      self$.cursor_get(cursor_op$SET_KEY, key)
    },
    seek = function(key) {
      self$.cursor_get(cursor_op$SET_RANGE, key)
    },

    del = function(dupdata = TRUE) {
      if (self$.valid) {
        res <- mdb_cursor_del(self$.ptr, dupdata)
        self$.txn$.mutations <- self$.txn$.mutations + 1L
        self$.cursor_get(cursor_op$GET_CURRENT)
        TRUE
      } else {
        FALSE
      }
    },
    put = function(key, value, dupdata = TRUE, overwrite = TRUE,
                   append = FALSE) {
      res <- mdb_cursor_put(self$.ptr, key, value, dupdata, overwrite, append)
      self$.txn$.mutations <- self$.txn$.mutations + 1L
      self$.cursor_get(cursor_op$GET_CURRENT)
      res
    },

    replace = function(key, value, as_raw = NULL) {
      res <- mdb_cursor_put(self$.ptr, key, value, TRUE, FALSE, FALSE)
      if (res) {
        ## No value existed previously
        return(NULL)
      }
      old_ptr <- mdb_cursor_get(self$.ptr, cursor_op$GET_CURRENT, NULL, NULL)
      old <- mdb_proxy_copy(old_ptr[[2L]], as_raw)
      self$put(key, value, TRUE, TRUE, FALSE)
      old
    },

    pop = function(key, as_raw = NULL) {
      if (self$move_to(key)) {
        old <- self$.cur_value$data(as_raw)
        self$del()
        old
      } else {
        NULL
      }
    },

    get = function(key, as_proxy = FALSE, as_raw = NULL) {
      self$move_to(key)
      self$value(as_proxy, as_raw)
    },

    ## These ones are only valid for databases with duplicates.  I
    ## think that I should be careful how I expose this.  One option
    ## is to use inheritance; a bit crap but might at least keep the
    ## interface fairly simple.
    first_dup = function() {
      key <- self$.cur_key
      v <- self$.cursor_get(cursor_op$FIRST_DUP)
      if (v) {
        self$.cur_key <- key
      }
      invisible(v)
    },
    last_dup = function() {
      key <- self$.cur_key
      v <- self$.cursor_get(cursor_op$LAST_DUP)
      if (v) {
        self$.cur_key <- key
      }
      invisible(v)
    },
    move_prev_dup = function() {
      self$.cursor_get(cursor_op$PREV_DUP)
    },
    move_next_dup = function() {
      self$.cursor_get(cursor_op$NEXT_DUP)
    },
    move_to_dup = function(key, value) {
      self$.cursor_get(cursor_op$GET_BOTH, key, value)
    },
    seek_dup = function(key, value) {
      self$.cursor_get(cursor_op$GET_BOTH_RANGE, key, value)
    },
    move_prev_nodup = function() {
      self$.cursor_get(cursor_op$PREV_NODUP)
    },
    move_next_nodup = function() {
      self$.cursor_get(cursor_op$NEXT_NODUP)
    },
    count = function() {
      mdb_cursor_count(self$.ptr)
    }
  ))
