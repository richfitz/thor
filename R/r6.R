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

R6_dbenv <- R6::R6Class(
  "dbenv",
  cloneable = FALSE,
  public = list(
    .ptr = NULL,
    .db = NULL,
    .dbs = new.env(parent = emptyenv()),
    .deps = NULL,
    .spare_txns = NULL,

    ## This argument list will likely grow to drop flags
    initialize = function(path, flags = NULL, reversekey = FALSE,
                          dupsort = FALSE, create = TRUE) {
      ## Be more user-friendly
      if (create && !file.exists(path)) {
        dir.create(path, FALSE, TRUE)
      }
      ## Core pointer:
      self$.ptr <- mdb_env_create()
      mdb_env_open(self$.ptr, path, flags)

      ## Bookkeeping:
      self$.spare_txns = stack()
      self$.deps = stack()
      ## self$open_database(NULL, NULL, reversekey, dupsort, create)
    },

    ## Informational
    set_mapsize = function(mapsize) {
      mdb_env_set_mapsize(self$.ptr, mapsize)
    },
    path = function() {
      mdb_env_get_path(self$.ptr)
    },
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
      ## TODO: in the python version the accounting is all done at the
      ## python level, where a map of dependents is kept around and
      ## then closed before activating running this.  That's a nice
      ## trick.  "Spare transactions", too, and dbs.  I'm doing all
      ## the hard work within C which might not be the best way of
      ## running it, really.
      mdb_env_close(self$.ptr)
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
        db <- with_new_txn(env$.ptr, newdb)
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

    begin = function(...) {
      ## TODO: python hands over env, db, parent, write, buffer - the
      ## latter is a python thing controlling how types are returned
      R6_transaction$new(...)
    }
  ))

R6_database <- R6::R6Class(
  "database",
  cloneable = FALSE,
  public = list(
    .ptr = NULL,
    initialize = function(env, txn_ptr, name, reversekey, dupsort, create) {
      self$ptr <- mdb_dbi_open(txn_ptr, name, reversekey, dupsort, create)
      env$.deps$add(self)
    },
    invalidate = function() {
      ## NOTE: We don't explicitly call close here; this is on purpose
      self$.ptr <- NULL
    }
  ))

R6_transaction <- R6::R6Class(
  "transaction",
  public = list(
    env = NULL,
    ptr = NULL,
    deps = NULL,
    initialize = function(env, db = NULL, parent = NULL, write = FALSE) {
      self$env <- env # hold ref - this is done on the C side though
      self$db <- db %||% env$db
      if (parent) {
        self$parent = parent
        ## Here we need something that will add by pointer so that we
        ## can stuff things into an environment rather than having to
        ## append things.  It's also possible that we don't need to do
        ## this at all...
        ##   parent$deps$add(self)
        ## No, it's necessary because we need to lookup
      }
      if (write) {
        ## if (env$readonly) {
        ##   ..error..
        ##}
        self$.ptr <- mdb_txn_begin(env$ptr, parent$ptr, NULL)
        self$write <- TRUE
      } else {
        if (env$spare_txns$is_empty()) {
          self$.ptr <- mdb_txn_begin(env$ptr, parent$ptr, flags_txn$RDONLY)
        } else {
          ptr <- env$spare_txns$pop()
          withCallingHandlers(mdb_txn_renew(ptr),
                              error = function(e) mdb_txn_abort(ptr))
          self$.ptr <- ptr
        }
      }
    },

    invalidate = function() {
      self$abort()
      ## self$env$deps$discard(self)
      self$.ptr <- NULL
      self$parent <- NULL
      self$env <- NULL
    },
    invalidate_deps = function() {
      ## TODO: work out how/if we do this...
    },

    ## NOTE: the python version allowed alternative dbs to be passed
    ## through via the cursor.  That might be necessary, so expand the
    ## arg lists to take db = NULL and sanitise/arrange as required.
    id = function() {
      mdb_txn_id(self$.ptr)
    },
    stat = function() {
      mdb_stat(self$dbi, self$db$ptr)
    },
    commit = function() {
      self$invalidate_deps()
      mdb_txn_commit(self$.ptr)
    },
    abort = function() {
      self$invalidate_deps()
      mdb_txn_abort(self$.ptr)
    },

    ## TODO: some work to do here...
    get = function(key) {
      mdb_dbi_get(self$.ptr, self$dbi$ptr, key)
    },
    put = function(key, data, flags = NULL) {
      mdb_dbi_put(self$.ptr, self$dbi$ptr, key, data, flags)
    },
    del = function(key, data = NULL) {
      mdb_dbi_del(self$.ptr, self$dbi$ptr, key, data)
    },

    cursor = function() {
      R6_cursor$new(self)
    }
  ))

R6_cursor <- R6::R6Class(
  "mdb_cursor",
  public = list(
    txn = NULL,
    dbi = NULL,
    ptr = NULL,
    initialize = function(txn) {
      self$txn <- txn
      self$dbi <- txn$dbi
      self$.ptr <- mdb_cursor_open(self$txn$ptr, self$dbi$ptr)

      ## This is necessary if we use dependency counting on the R side:
      ##   self$txn$deps$add(self)
      ##   self$dbi$deps$add(self)
    },

    invalidate = function() {
      self$close()
      ##   self$txn$deps$discard(self)
      ##   self$dbi$deps$discard(self)
    },

    close = function() {
      mdb_cursor_close(self$.ptr)
      self$txn <- NULL
      self$dbi <- NULL
    },

    ## This might not be ideal.  There are some other ways forward
    ## here - see the python interface in particular.
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
with_new_txn <- function(env_ptr, f, parent = NULL, flags = NULL) {
  txn <- mdb_txn_begin(env_ptr)
  withCallingHandlers({
    ret <- f(txn)
    mdb_txn_commit(txn)
    ret
  }, error = function(e) mdb_txn_abort(txn))
}
