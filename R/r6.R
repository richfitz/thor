## The names here are nasty!  I think I'd like some names that are a
## bit nicer for end users, since this is the bit of the package that
## people will actually interact with!
##
##   dbenv
##   dbhandle
##   transaction
##   cursor
##
## or
##
##   Environment : env
##   Database : db
##   Transaction : txn
##   Cursor : cur

## For print methods,
## * indicate if .ptr is NULL or not
## * indicate dependents
## * hide all dots (and the print function)

dbenv <- function(path, flags = NULL,
                  maxdbs = NULL, maxreaders = NULL, mapsize = NULL,
                  reversekey = FALSE, dupsort = FALSE, create = TRUE) {
  R6_dbenv$new(path, flags,
               maxdbs = maxdbs, maxreaders = maxreaders, mapsize = mapsize,
               reversekey = reversekey, dupsort = dupsort, create = create)
}

## TODO: Make a global cache of environment handles (or a lock) so
## that if we open a database multiple times we will prevent a
## deadlock.  However, we can't do that in a really trivial way
## because we'd need to be very careful about close.  So a proxy
## object (like the deps) would need to be kept around that keeps
## track of the write transaction bit.  For now it's not done.
R6_dbenv <- R6::R6Class(
  "dbenv",
  cloneable = FALSE,
  public = list(
    .ptr = NULL,
    .db = NULL,
    .dbs = new.env(parent = emptyenv()),
    .deps = NULL,
    .write_txn = NULL,

    ## This argument list will likely grow to drop flags
    initialize = function(path, flags = NULL,
                          maxdbs = NULL, maxreaders = NULL, mapsize = NULL,
                          reversekey = FALSE, dupsort = FALSE, create = TRUE) {
      self$.deps = stack()
      self$.ptr <- mdb_env_create()

      ## TODO: throughout here, nicer conversion to integer
      if (!is.null(maxreaders)) {
        mdb_env_set_maxreaders(self$.ptr, as.integer(maxreaders))
      }
      if (!is.null(maxdbs)) {
        mdb_env_set_maxdbs(self$.ptr, as.integer(maxdbs))
      }
      if (!is.null(mapsize)) {
        mdb_env_set_mapsize(self$.ptr, as.integer(mapsize))
      }

      ## Be more user-friendly
      if (create && !file.exists(path)) {
        dir.create(path, FALSE, TRUE)
      }

      mdb_env_open(self$.ptr, path, flags_env$NOTLS | flags)

      self$open_database(NULL, NULL, reversekey, dupsort, create)
    },

    finalize = function() {
      message("[GC] env")
      self$close()
    },

    path = function() {
      mdb_env_get_path(self$.ptr)
    },
    ## This one needs work I think...
    flags = function() {
      mdb_env_get_flags(self$.ptr)
    },
    stat = function() {
      mdb_env_stat(self$.ptr)
    },
    info = function() {
      mdb_env_info(self$.ptr)
    },
    maxkeysize = function() {
      mdb_env_get_maxkeysize(self$.ptr)
    },
    maxreaders = function() {
      mdb_env_get_maxreaders(self$.ptr)
    },

    readers = function() {
      mdb_reader_list(self$.ptr)
    },
    reader_check = function() {
      mdb_reader_check(self$.ptr)
    },

    copy = function(path, compact = FALSE) {
      if (!file.exists(path)) {
        dir.create(path, FALSE, TRUE)
      }
      mdb_env_copy(self$.ptr, path, compact)
      invisible(path)
    },

    sync = function(force = FALSE) {
      mdb_env_sync(self$.ptr, force)
    },

    close = function() {
      if (!is.null(self$.ptr)) {
        invalidate_dependencies(self)
        mdb_env_close(self$.ptr)
        self$.db <- NULL
        self$.dbs <- NULL
        self$.ptr <- NULL
      }
    },

    open_database = function(key = NULL, txn = NULL, reversekey = FALSE,
                             dupsort = FALSE, create = TRUE) {
      db <- if (is.null(key)) self$.db else self$.dbs[[key]]
      if (!is.null(db)) {
        return(db)
      }

      newdb <- function(txn) {
        R6_database$new(self, txn, key, reversekey, dupsort, create)
      }
      if (is.null(txn)) {
        db <- with_new_txn(self, newdb, write = TRUE)
      } else {
        db <- newdb(txn)
      }

      if (is.null(key)) {
        self$.db <- db
      } else {
        self$.dbs[[key]] <- db
      }
      invisible(db)
    },

    begin = function(db = NULL, parent = NULL, write = FALSE) {
      R6_transaction$new(self, db, parent, write)
    }

    ## this might be nice - not quite there though
    ## get = function(...) {
    ##   with_new_txn(self, function(txn) txn$get(...), write = FALSE)
    ## },
    ## put = function(...) {
    ##   with_new_txn(self$.ptr, function(txn) txn$put(...))
    ## },
    ## del = function(...) {
    ##   with_new_txn(self$.ptr, function(txn) txn$del(...))
    ## }
  ))

## TODO: Add to the above wrappers for get/put/etc that use a
## temporary transaction?

R6_database <- R6::R6Class(
  "database",
  cloneable = FALSE,
  public = list(
    .ptr = NULL,
    initialize = function(env, txn_ptr, name, reversekey, dupsort, create) {
      self$.ptr <- mdb_dbi_open(txn_ptr, name, reversekey, dupsort, create)
      env$.deps$add(self)
    },
    invalidate = function() {
      ## NOTE: We don't explicitly call close here; this is on purpose
      self$.ptr <- NULL
    },
    id = function() {
      mdb_dbi_id(self$.ptr)
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
    .parent = NULL,
    .write = NULL,

    initialize = function(env, db = NULL, parent = NULL, write = FALSE) {
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
      self$.write <- TRUE

      if (!is.null(parent)) {
        assert_is(parent, "transaction")
        self$.parent = parent
        parent$.deps$add(self)
      }

      if (write) {
        if (!is.null(env$.write_txn)) {
          stop("Write transaction is already active for this environment")
        }
        self$.ptr <- mdb_txn_begin(env$.ptr, parent$.ptr, rdonly = FALSE)
        env$.write_txn <- self
      } else {
        self$.ptr <- mdb_txn_begin(env$.ptr, parent$.ptr, rdonly = TRUE)
      }
    },

    invalidate = function() {
      ## Remove ourselves from upstream things

      ## TODO: This is a very different approach taken to the python
      ## library where invalidate calls these things but abort does
      ## not

      self$abort()
    },

    finalize = function() {
      message("[GC] transaction")
      if (!is.null(self$.ptr)) {
        self$abort()
      }
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
      message("...committing transaction")
      mdb_txn_commit(self$.ptr)
      self$.cleanup()
    },
    abort = function() {
      if (!is.null(self$.ptr)) {
        message("...aborting transaction")
        ##
        tryCatch(
          mdb_txn_abort(self$.ptr),
          error = function(e) message("error cleaning up but pressing on"))
        self$.cleanup()
      }
    },
    .cleanup = function() {
      if (!is.null(self$.parent)) {
        self$.parent$.deps$discard(self) # needed?
        self$.parent <- NULL
      }
      self$.env$.deps$discard(self)
      if (self$.write) {
        self$.env$.write_txn <- NULL
      }
      invalidate_dependencies(self)
      self$.db <- NULL
      self$.env <- NULL
      self$.ptr <- NULL
    },

    ## TODO: some work to do here:
    ##   - proxy object
    ##   - append, overwrite, dupdata on put
    ##   - replace, pop
    ##   - deletion gets MDB_NOTFOUND detection
    ##   - get should be able to throw (or not)
    get = function(key, missing_value = NULL, proxy = FALSE) {
      mdb_get(self$.ptr, self$.db$.ptr, key, missing_value, proxy)
    },
    put = function(key, data, flags = NULL) {
      mdb_put(self$.ptr, self$.db$.ptr, key, data, flags)
    },
    del = function(key, data = NULL) {
      mdb_del(self$.ptr, self$.db$.ptr, key, data)
    },

    cursor = function() {
      R6_cursor$new(self)
    }
  ))

R6_cursor <- R6::R6Class(
  "cursor",
  public = list(
    .txn = NULL,
    .db = NULL,
    .ptr = NULL,
    initialize = function(txn) {
      self$.txn <- txn
      self$.db <- txn$.db
      self$.ptr <- mdb_cursor_open(self$.txn$.ptr, self$.db$.ptr)
      txn$.deps$add(self)
      ## TODO: To implement mdb_drop we need to keep track of
      ## db->cursor links too
      ##   self$.db$.deps$add(self)
    },

    finalize = function() {
      message("[GC] env")
      self$invalidate()
    },

    invalidate = function() {
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

    ## This might not be ideal.  There are some other ways forward
    ## here - see the python interface in particular.  Supporting key,
    ## value, item, etc would be nice.  Knowing when the items need to
    ## be refreshed might also be nice.  Lots to do!
    get = function(key, op) {
      mdb_cursor_get(self$.ptr, key, op)
    },
    put = function(key, data, flags = NULL) {
      mdb_cursor_put(self$.ptr, key, data, flags)
    },
    del = function(nodupdata = FALSE) {
      mdb_cursor_del(self$.ptr, nodupdata)
    },
    count = function() {
      mdb_cursor_count(self$.ptr)
    }
  ))

## Helper function
with_new_txn <- function(env, f, parent = NULL, write = FALSE) {
  if (write) {
    if (!is.null(env$.write_txn)) {
      stop("FIXME")
    }
    txn <- mdb_txn_begin(env$.ptr, parent, FALSE)
    withCallingHandlers({
      ret <- f(txn)
      mdb_txn_commit(txn)
      ret
    }, error = function(e) mdb_txn_abort(txn))
  } else {
    ## Consider using the pool system in env?
    txn <- mdb_txn_begin(env$.ptr, parent, TRUE)
    withCallingHandlers({
      f(txn)
    }, finally = function(e) mdb_txn_abort(txn))
  }
}

invalidate_dependencies <- function(x) {
  message(sprintf("invalidating deps for a %s", class(x)[[1]]))
  if (!is.null(x$.deps)) {
    deps <- x$.deps$get()
    message(sprintf("%d deps", length(deps)))
    for (d in x$.deps$get()) {
      message(sprintf("...invalidating a %s", class(d)[[1]]))
      d$invalidate()
    }
    x$.deps <- NULL
  } else {
    message("(null deps)")
  }
}
