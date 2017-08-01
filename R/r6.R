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
##
## or
##
##   mdb_env
##   mdb_dbi
##   mdb_txn
##   mdb_cursor
##
## which is at least very simple and clear and mirrors the C api nicely

## For print methods:
##
## * indicate if .ptr is NULL or not
## * indicate dependents (there's a tree issue here; that's
##   fairly easy to deal with).
## * hide all dots (and the print function)
dbenv <- function(path, ..., mode = as.octmode("644"),
                  ## flags for env
                  subdir = TRUE, sync = TRUE, rdonly = FALSE,
                  metasync = TRUE, writemap = FALSE, lock = TRUE,
                  mapasync = FALSE, rdahead = TRUE, meminit = TRUE,
                  ## other args
                  maxdbs = NULL, maxreaders = NULL, mapsize = NULL,
                  reversekey = FALSE, dupsort = FALSE, create = TRUE) {
  R6_dbenv$new(path, mode,
               ## flags:
               subdir = subdir, sync = sync, rdonly = rdonly,
               metasync = metasync, writemap = writemap, lock = lock,
               mapasync = mapasync, rdahead = rdahead,
               meminit = meminit,
               ## other:
               maxdbs = maxdbs, maxreaders = maxreaders, mapsize = mapsize,
               reversekey = reversekey, dupsort = dupsort, create = create)
}

## TODO: Make a global cache of environment handles (or a lock) so
## that if we open a database multiple times we will prevent a
## deadlock.  However, we can't do that in a really trivial way
## because we'd need to be very careful about close.  So a proxy
## object (like the deps) would need to be kept around that keeps
## track of the write transaction bit.  For now it's not done.

write_txns <- new.env(parent = emptyenv())

R6_dbenv <- R6::R6Class(
  "dbenv",
  cloneable = FALSE,
  public = list(
    .ptr = NULL,
    .db = NULL,
    .dbs = NULL,
    .deps = NULL,
    .write_txn = NULL,
    .spare_txns = NULL,
    .path = NULL,

    initialize = function(path, mode,
                          subdir, sync, rdonly,
                          metasync, writemap, lock,
                          mapasync, rdahead, meminit,
                          maxdbs, maxreaders, mapsize,
                          reversekey, dupsort, create) {
      self$.deps = stack()
      self$.ptr <- mdb_env_create()
      self$.dbs <- new.env(parent = emptyenv())
      self$.spare_txns = stack()

      if (!is.null(maxreaders)) {
        mdb_env_set_maxreaders(self$.ptr, maxreaders)
      }
      if (!is.null(maxdbs)) {
        mdb_env_set_maxdbs(self$.ptr, maxdbs)
      }
      if (!is.null(mapsize)) {
        mdb_env_set_mapsize(self$.ptr, mapsize)
      }

      ## Be more user-friendly
      if (create && subdir && !file.exists(path)) {
        ## TODO: here we get silly error messages if path is not a
        ## reasonable thing.
        dir.create(path, FALSE, TRUE)
      }
      mdb_env_open(self$.ptr, path, mode,
                   subdir, sync, rdonly,
                   metasync, writemap, lock,
                   mapasync, rdahead, meminit)

      self$.path <- normalizePath(self$path(), mustWork = TRUE)
      self$open_database(NULL, reversekey, dupsort, create)
    },

    .check_write = function() {
      if (exists(self$.path, write_txns)) {
        if (!is.null(self$.write_txn)) {
          stop("Write transaction is already active for this environment")
        } else {
          stop("Write transaction is already active for this path")
        }
      }
    },

    .new_txn_ptr = function(write, parent_ptr, temporary = FALSE) {
      if (write) {
        self$.check_write()
        ptr <- mdb_txn_begin(self$.ptr, parent_ptr, rdonly = FALSE)
        if (!temporary) {
          write_txns[[self$.path]] <- self$.ptr
          self$.write_txn <- ptr
        }
      } else {
        ptr <- self$.spare_txns$pop()
        if (is.null(ptr)) {
          ptr <- mdb_txn_begin(self$.ptr, parent_ptr, rdonly = TRUE)
        } else {
          mdb_txn_renew(ptr)
        }
      }
      ptr
    },

    finalize = function() {
      self$close()
    },

    format = function() {
      format_thor(self)
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

    set_mapsize = function(size) {
      ## assert_integer_like(size)
      mdb_env_set_mapsize(self$.ptr, as_integer(size))
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
        for (txn_ptr in rev(self$.spare_txns$get())) {
          mdb_txn_abort(txn_ptr, FALSE)
        }
        self$.spare_txns <- NULL
        mdb_env_close(self$.ptr)
        self$.db <- NULL
        self$.dbs <- NULL
        self$.ptr <- NULL
      }
    },

    ## NOTE: The python interface allowed re-using a write transaction
    ## here.  However doing that risks leaving a broken db floating
    ## around in the case of an aborted transaction.  A possible
    ## solution would be to add the db to the transaction's
    ## dependencies in this case but that's not quite right either
    ## because then commit would invalidate the db.  So the simplest
    ## case is to require that the db is created and commited before
    ## anyone uses it.
    open_database = function(key = NULL, reversekey = FALSE, dupsort = FALSE,
                             create = TRUE) {
      db <- if (is.null(key)) self$.db else self$.dbs[[key]]
      if (!is.null(db)) {
        return(db)
      }

      newdb <- function(txn_ptr) {
        R6_database$new(self, txn_ptr, key, reversekey, dupsort, create)
      }
      db <- with_new_txn(self, TRUE, newdb)

      if (is.null(key)) {
        self$.db <- db
      } else {
        self$.dbs[[key]] <- db
      }
      invisible(db)
    },

    drop_database = function(db, delete = TRUE) {
      assert_is(db, "database")
      name <- db$.name
      if (is.null(name)) {
        stop("Can't delete root database")
      }
      if (!identical(self$.dbs[[name]], db)) {
        stop("this is not our database")
      }

      dropdb <- function(txn_ptr) {
        for (x in self$.deps$get()) {
          if (inherits(x, "transaction") && identical(x$.db, db)) {
            x$.invalidate()
            self$.deps$discard(x)
          }
        }
        mdb_drop(txn_ptr, db$.ptr, delete)
        rm(list = name, envir = self$.dbs)
        self$.deps$discard(db)
        db$.invalidate()
      }

      with_new_txn(self, TRUE, dropdb)
    },

    begin = function(db = NULL, write = FALSE) {
      R6_transaction$new(self, db, write)
    },

    destroy = function() {
      path <- self$path()
      subdir <- self$flags()[["subdir"]]
      self$close()
      if (subdir) {
        unlink(path, recursive = TRUE)
      } else {
        file.remove(c(path, paste0(path, "-lock")))
      }
    },

    ## Big group of helper methods; these will not necessarily be as
    ## efficient as using a slightly longer lived transaction.
    get = function(key, missing_is_error = TRUE, as_raw = NULL, db = NULL) {
      db <- db %||% self$.db
      with_new_txn(self, FALSE, function(txn_ptr)
        mdb_get(txn_ptr, db$.ptr, key, missing_is_error, FALSE, as_raw))
    },
    mget = function(key, as_raw = NULL, db = NULL) {
      db <- db %||% self$.db
      with_new_txn(self, FALSE, function(txn_ptr)
        thor_mget(txn_ptr, db$.ptr, key, FALSE, as_raw))
    },
    put = function(key, value, dupdata = TRUE, overwrite = TRUE,
                   append = FALSE, db = NULL) {
      db <- db %||% self$.db
      with_new_txn(self, TRUE, function(txn_ptr)
        mdb_put(txn_ptr, db$.ptr, key, value, dupdata, overwrite, append))
    },
    mput = function(key, value, dupdata = TRUE, overwrite = TRUE,
                   append = FALSE, db = NULL) {
      db <- db %||% self$.db
      with_new_txn(self, TRUE, function(txn_ptr)
        thor_mput(txn_ptr, db$.ptr, key, value, dupdata, overwrite, append))
    },
    del = function(key, value = NULL, db = NULL) {
      db <- db %||% self$.db
      if (!is.null(value) && !db$.dupsort) {
        stop("'value' is not allowed for databases with dupsort = FALSE")
      }
      with_new_txn(self, TRUE, function(txn_ptr)
        mdb_del(txn_ptr, db$.ptr, key, value))

    },
    mdel = function(key, value = NULL, db = NULL) {
      db <- db %||% self$.db
      if (!is.null(value) && !db$.dupsort) {
        stop("'value' is not allowed for databases with dupsort = FALSE")
      }
      with_new_txn(self, TRUE, function(txn_ptr)
        thor_mdel(txn_ptr, db$.ptr, key, value))

    },
    exists = function(key, db = NULL) {
      db <- db %||% self$.db
      with_new_txn(self, FALSE, function(txn_ptr)
        thor_exists(txn_ptr, db$.ptr, key))
    },
    list = function(starts_with = NULL, as_raw = FALSE, size = NULL,
                    db = NULL) {
      db <- db %||% self$.db
      with_new_txn(self, FALSE, function(txn_ptr) {
        cur_ptr <- mdb_cursor_open(txn_ptr, db$.ptr)
        on.exit(mdb_cursor_close(cur_ptr))
        thor_list(cur_ptr, starts_with, as_raw, size)
      })
    },
    replace = function(key, value, as_raw = NULL, db = NULL) {
      db <- db %||% self$.db
      with_new_txn_object(self, db, TRUE, function(txn)
        txn$replace(key, value, as_raw))
    },
    pop = function(key, as_raw = NULL, db = NULL) {
      db <- db %||% self$.db
      with_new_txn_object(self, db, TRUE, function(txn)
        txn$pop(key, as_raw))
    }
  ))

## TODO: Add to the above wrappers for get/put/etc that use a
## temporary transaction?

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
