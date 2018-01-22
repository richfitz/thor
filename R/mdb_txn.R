##' Transactions are required for every mdb operation.  Even when
##' using the convenience functions in \code{\link{mdb_env}}
##' (\code{get}, etc), a transaction is created and committed each
##' time.  Within a transaction, either everything happens or nothing
##' happens, and everything gets a single consistent view of the
##' database.
##'
##' There can be many read transactions per environment, but only one
##' write transactions.  Because R is single-threaded, that means that
##' you can only simultaneously write from an mdb environment from a
##' single object - any further attempts to open write transactions it
##' would block forever while waiting for a lock that can't be
##' released because there is only one thread!
##'
##' @template mdb_txn
##'
##' @title Use mdb transactions
##' @rdname mdb_txn
##' @aliases mdb_txn
##' @name mdb_txn
##' @examples
##' # Start by creating a new environment, and within that a write
##' # transaction
##' env <- thor::mdb_env(tempfile())
##' txn <- env$begin(write = TRUE)
##'
##' # With this transaction we can write values and see them as set
##' txn$put("a", "hello")
##' txn$get("a")
##'
##' # But because the transaction is not committed, any new
##' # transaction will not see the values:
##' env$get("a", missing_is_error = FALSE) # NULL
##' txn2 <- env$begin()
##' txn2$get("a", missing_is_error = FALSE) # NULL
##'
##' # Once we commit a transaction, *new* transactions will see the
##' # value
##' txn$commit()
##' env$get("a") # "hello"
##' env$begin()$get("a") # "hello"
##'
##' # But old transactions retain their consistent view of the database
##' txn2$get("a", missing_is_error = FALSE)
##'
##' # Cleanup
##' env$destroy()
NULL

R6_mdb_txn <- R6::R6Class(
  "mdb_txn",
  cloneable = FALSE,
  public = list(
    .env = NULL,
    .ptr = NULL,
    .db = NULL,
    .deps = NULL,
    .write = NULL,
    .mutations = 0L,
    .methods = list(
      Informational = c("id", "stat"),
      Finish = c("commit", "abort"),
      Cursors = "cursor",
      Data = c("get", "put", "del", "exists", "list",
               "mget", "mput", "mdel",
               "replace", "pop"),
      Compare = c("cmp", "dcmp")),

    initialize = function(env, db, write, sync, metasync) {
      ## If the R6 issue is not a bug then we don't have to store
      ## upstream references for GC purposes - just if we need to use
      ## them!
      self$.env <- env
      env$.deps$add(self)

      if (is.null(db)) {
        db <- env$.db
      } else {
        assert_is(db, "mdb_dbi")
      }
      self$.db <- db
      self$.deps <- stack()
      self$.write <- write

      ## NOTE: Parent transactions are not supported yet
      parent <- NULL
      self$.ptr <- env$.new_txn_ptr(write, parent, sync, metasync)
    },

    .cache_spare = function() {
      if (!self$.write) {
        mdb_txn_reset(self$.ptr)
        self$.env$.spare_txns$push(self$.ptr)
        self$.cleanup()
        TRUE
      } else {
        FALSE
      }
    },

    .invalidate = function() {
      ## Remove ourselves from upstream things

      ## TODO: This is a very different approach taken to the python
      ## library where invalidate calls these things but abort does
      ## not

      self$abort(FALSE)
    },

    finalize = function() {
      if (!is.null(self$.ptr)) {
        self$abort()
      }
    },

    format = function() {
      format_thor(self)
    },

    ## NOTE: the python version allowed alternative dbs to be passed
    ## through via the cursor.  That might be necessary, so expand the
    ## arg lists to take db = NULL and sanitise/arrange as required.
    id = function() {
      mdb_txn_id(self$.ptr)
    },
    stat = function() {
      mdb_stat(self$.ptr, self$.db$.ptr)
    },
    commit = function() {
      invalidate_dependencies(self)
      if (!self$.cache_spare()) {
        mdb_txn_commit(self$.ptr)
        self$.cleanup()
      }
    },
    abort = function(cache = TRUE) {
      if (!is.null(self$.ptr)) {
        invalidate_dependencies(self)
        if (!(cache && self$.cache_spare())) {
          mdb_txn_abort(self$.ptr, FALSE)
          self$.cleanup()
        }
      }
    },
    .cleanup = function() {
      self$.env$.deps$discard(self)
      if (self$.write) {
        rm(list = self$.env$.path, envir = write_txns)
        self$.env$.write_txn <- NULL
      }
      self$.db <- NULL
      self$.env <- NULL
      self$.ptr <- NULL
      self$.mutations <- Inf
    },

    get = function(key, missing_is_error = TRUE,
                   as_proxy = FALSE, as_raw = NULL) {
      res <- mdb_get(self$.ptr, self$.db$.ptr, key, missing_is_error,
                     as_proxy, as_raw)
      if (as_proxy) {
        mdb_val_proxy(self, res)
      } else {
        res
      }
    },
    put = function(key, value, dupdata = TRUE, overwrite = TRUE,
                   append = FALSE) {
      self$.mutations <- self$.mutations + 1L
      mdb_put(self$.ptr, self$.db$.ptr, key, value, dupdata, overwrite, append)
    },
    del = function(key, value = NULL) {
      if (!is.null(value) && !self$.db$.dupsort) {
        stop("'value' is not allowed for databases with dupsort = FALSE")
      }
      self$.mutations <- self$.mutations + 1L
      mdb_del(self$.ptr, self$.db$.ptr, key, value)
    },

    exists = function(key) {
      thor_exists(self$.ptr, self$.db$.ptr, key)
    },
    list = function(starts_with = NULL, as_raw = FALSE, size = NULL) {
      cur_ptr <- mdb_cursor_open(self$.ptr, self$.db$.ptr)
      on.exit(mdb_cursor_close(cur_ptr))
      thor_list(cur_ptr, starts_with, as_raw, size)
    },

    replace = function(key, value, as_raw = NULL) {
      cur <- self$cursor()
      on.exit(cur$close())
      cur$replace(key, value, as_raw)
    },
    pop = function(key, as_raw = NULL) {
      cur <- self$cursor()
      on.exit(cur$close())
      cur$pop(key, as_raw)
    },

    mget = function(key, as_proxy = FALSE, as_raw = NULL) {
      res <- thor_mget(self$.ptr, self$.db$.ptr, key, as_proxy, as_raw)
      if (as_proxy) {
        lapply(res, mdb_val_proxy, txn = self)
      } else {
        res
      }
    },
    mput = function(key, value, dupdata = TRUE, overwrite = TRUE,
                    append = FALSE) {
      self$.mutations <- self$.mutations + 1L
      thor_mput(self$.ptr, self$.db$.ptr, key, value,
                dupdata, overwrite, append)
    },
    mdel = function(key, value = NULL) {
      if (!is.null(value) && !self$.db$.dupsort) {
        stop("'value' is not allowed for databases with dupsort = FALSE")
      }
      self$.mutations <- self$.mutations + 1L
      thor_mdel(self$.ptr, self$.db$.ptr, key, value)
    },

    cursor = function() {
      R6_mdb_cursor$new(self)
    },

    cmp = function(a, b) {
      mdb_cmp(self$.ptr, self$.db$.ptr, a, b)
    },
    dcmp = function(a, b) {
      if (self$.db$.dupsort) {
        mdb_dcmp(self$.ptr, self$.db$.ptr, a, b)
      } else {
        stop("dcmp() is not meaningful on database with dupsort = FALSE")
      }
    }
  ))
