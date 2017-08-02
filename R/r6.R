R6_database <- R6::R6Class(
  "database",
  cloneable = FALSE,
  public = list(
    .ptr = NULL,
    .dupsort = NULL,
    .name = NULL,
    ## TODO: other options here include dupfixed, integerkey,
    ## integerdup, reversedup
    initialize = function(env, txn_ptr, name, reversekey, dupsort, create) {
      self$.ptr <- mdb_dbi_open(txn_ptr, name, reversekey, dupsort, create)
      env$.deps$add(self)
      self$.dupsort <- dupsort
      self$.name <- name
    },
    .invalidate = function() {
      ## NOTE: We don't explicitly call close here, following the
      ## docs.  I think we could consider doing better though - if we
      ## have a situation where there are no depenendents in the
      ## parent we can close fairly easily.  We can also arrange to
      ## invalidate things where that is useful because we have it
      ## already.  This way around we don't free up handles which is
      ## not perfect.
      self$.ptr <- NULL
    },
    format = function() {
      format_thor(self)
    },
    id = function() {
      mdb_dbi_id(self$.ptr)
    },
    flags = function(txn) {
      assert_is(txn, "transaction")
      mdb_dbi_flags(txn$.ptr, self$.ptr)
    }
  ))

## TODO: We're only allowed a single write transaction per
## database/environment so we should store that there.
##
## Be careful not to allow reset/renew on anything other than readonly
R6_transaction <- R6::R6Class(
  "transaction",
  public = list(
    .env = NULL,
    .ptr = NULL,
    .db = NULL,
    .deps = NULL,
    .write = NULL,
    .mutations = 0L,

    initialize = function(env, db, write) {
      ## If the R6 issue is not a bug then we don't have to store
      ## upstream references for GC purposes - just if we need to use
      ## them!
      self$.env <- env
      env$.deps$add(self)

      if (is.null(db)) {
        db <- env$.db
      } else {
        assert_is(db, "database")
      }
      self$.db <- db
      self$.deps <- stack()
      self$.write <- write

      ## NOTE: Parent transactions are not supported yet
      parent <- NULL
      self$.ptr <- env$.new_txn_ptr(write, parent)
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
      self$.mutations = self$.mutations + 1L
      mdb_put(self$.ptr, self$.db$.ptr, key, value, dupdata, overwrite, append)
    },
    del = function(key, value = NULL) {
      if (!is.null(value) && !self$.db$.dupsort) {
        stop("'value' is not allowed for databases with dupsort = FALSE")
      }
      self$.mutations = self$.mutations + 1L
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
      self$.mutations = self$.mutations + 1L
      thor_mput(self$.ptr, self$.db$.ptr, key, value,
                dupdata, overwrite, append)
    },
    mdel = function(key, value = NULL) {
      if (!is.null(value) && !self$.db$.dupsort) {
        stop("'value' is not allowed for databases with dupsort = FALSE")
      }
      self$.mutations = self$.mutations + 1L
      thor_mdel(self$.ptr, self$.db$.ptr, key, value)
    },

    cursor = function() {
      R6_cursor$new(self)
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

R6_cursor <- R6::R6Class(
  "cursor",
  public = list(
    .txn = NULL,
    .db = NULL,
    .ptr = NULL,
    .cur_key = NULL,
    .cur_value = NULL,
    .valid = FALSE,

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

    ## TODO: pass value_if_missing through to value and make size NA for these?
    key = function(as_proxy = FALSE, as_raw = NULL) {
      if (is.null(self$.cur_key) ||!self$.cur_key$is_valid()) {
        self$.cursor_get(cursor_op$GET_CURRENT)
      }
      if (as_proxy) {
        self$.cur_key
      } else {
        self$.cur_key$data(as_raw)
      }
    },

    value = function(as_proxy = FALSE, as_raw = NULL) {
      if (is.null(self$.cur_value) ||!self$.cur_value$is_valid()) {
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
    put = function(key, value, dupdata = TRUE, overwrite = TRUE, append = FALSE) {
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

## Helper function
with_new_txn <- function(env, write, f) {
  txn_ptr <- env$.new_txn_ptr(write, NULL, TRUE)
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

with_new_txn_object <- function(env, db, write, f) {
  txn <- env$begin(db, write)
  withCallingHandlers({
    ret <- f(txn)
    txn$commit()
    ret
  }, error = function(e) txn$abort())
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

format_thor <- function(x) {
  exclude <- c("initialize", "finalize", "format")
  method_names <- setdiff(ls(x, pattern = "^[^.]"), exclude)
  methods <- vapply(method_names, function(i) capture_args(x[[i]], i),
                    character(1), USE.NAMES = FALSE)
  paste(c(sprintf("<%s>", class(x)[[1]]),
          "  Public:",
          sprintf("    %s", methods)),
        collapse = "\n")
}

##' @export
print.mdb_val_proxy <- function(x, ...) {
  cat(paste0(format_thor(x), "\n"))
}
