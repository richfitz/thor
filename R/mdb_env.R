##' Create a \code{mdb_env} "environment" object.  This is the way
##' that interacts with a lmdb database and once created, includes
##' methods for querying the environment, creating databases, starting
##' transactions and (through those) adding, getting and removing
##' data.  This page includes \emph{reference} documentation for the
##' object and readers are first directed to the vignette
##' (\code{vignette("thor"}).
##'
##' The \code{thor} package is a wrapper around \code{lmdb} and so
##' below I have provided pointers to relevant options in \code{lmdb}
##' - the wrapper is fairly thin and so picks up limitations and
##' restrictions from the underlying library.  Some portions of the
##' documentation here derives from the lmdb source documentation -
##' the file lmdb.h in particular.
##'
##' @title Create an mdb_env environment
##'
##' @param path The directory in which the database files will reside.
##'   If \code{create} is \code{TRUE} this path will be created for
##'   you if it does not exist (in contrast with the \code{lmdb} C
##'   API).  If \code{subdir} is \code{FALSE} this is the path to the
##'   database file and an additional lock file will be created by
##'   appending "-lock" to \code{path}.
##'
##' @param mode The file mode (UNIX file permissions) to set on
##'   created files.  this must be an \code{octmode} object, with the
##'   default (\code{as.octmode("644"}) being user-writable and
##'   world-readable.
##'
##' @param subdir By default, lmdb creates its files wthin a directory
##'   (at \code{path}).  If \code{subdir = FALSE} then the \code{path}
##'   is interpreted as the path to the main database file and a lock
##'   file will be created with "-lock" appended to the filename.
##'   Passing \code{subdir = FALSE} is equivalent to lmdb's
##'   \code{MDB_NOSUBDIR} flag.
##'
##' @param readonly Open the environment in read-only mode.  No write
##'   operations are allowed.  LMDB will still modify the lock file.
##'   Passing \code{readonly = TRUE} is equivalent to lmdb's
##'   \code{MDB_RDONLY} flag.
##'
##' @param metasync If \code{FALSE}, flush system buffers to disk only
##'   once per transaction, omit the metadata flush. Defer that until
##'   the system flushes files to disk, or next commit or the next
##'   call to the \code{$sync()} method.  This optimization maintains
##'   database integrity, but a system crash may undo the last
##'   committed transaction. I.e. it preserves the ACI (atomicity,
##'   consistency, isolation) but not D (durability) database
##'   property.  Passing \code{metasync = FALSE} is equivalent to
##'   lmdb's \code{MDB_NOMETASYNC} flag.
##'
##' @param sync If \code{FALSE}, don't flush system buffers to disk
##'   when committing a transaction.  This optimization means a system
##'   crash can corrupt the database or lose the last transactions if
##'   buffers are not yet flushed to disk.  The risk is governed by
##'   how often the system flushes dirty buffers to disk and how often
##'   the \code{$sync()} method is called.  However, if the filesystem
##'   preserves write order and \code{writemap = FALSE}, transactions
##'   exhibit ACI (atomicity, consistency, isolation) properties and
##'   only lose D (durability).  I.e. database integrity is
##'   maintained, but a system crash may undo the final transactions.
##'   Note that \code{sync = FALSE, writemap = TRUE} leaves the system
##'   with no hint for when to write transactions to disk, unless
##'   \code{$sync()} is called.  \code{map_async = TRUE, writemap =
##'   TRUE} may be preferable.  Passing \code{sync = FALSE} is
##'   equivalent to lmdb's \code{MDB_NOSYNC} flag.
##'
##' @param writemap If \code{TRUE}, use a writeable memory map unless
##'   \code{readonly = TRUE} is set. This uses fewer mallocs but loses
##'   protection from application bugs like wild pointer writes and
##'   other bad updates into the database. This may be slightly faster
##'   for DBs that fit entirely in RAM, but is slower for DBs larger
##'   than RAM. Incompatible with nested transactions. Do not mix
##'   processes with \code{writemap = TRUE} and \code{writemap =
##'   FALSE} on the same environment.  This can defeat durability
##'   (\code{$sync()} etc).  Passing \code{writemap = TRUE} is
##'   equivalent to lmdb's \code{MDB_WRITEMAP} flag.
##'
##' @param lock If \code{FALSE}, don't do any locking. If concurrent
##'   access is anticipated, the caller must manage all concurrency
##'   itself. For proper operation the caller must enforce
##'   single-writer semantics, and must ensure that no readers are
##'   using old transactions while a writer is active. The simplest
##'   approach is to use an exclusive lock so that no readers may be
##'   active at all when a writer begins.  Passing \code{lock = FALSE}
##'   is equivalent to lmdb's \code{MDB_NOLOCK} flag.
##'
##' @param mapasync If \code{TRUE}, When using \code{writemap = TRUE},
##'   use asynchronous flushes to disk.  As with \code{sync = FALSE},
##'   a system crash can then corrupt the database or lose the last
##'   transactions. Calling \code{$sync()} ensures on-disk database
##'   integrity until next commit.  Passing \code{mapasync = FALSE} is
##'   equivalent to lmdb's \code{MDB_MAPASYNC} flag.
##'
##' @param rdahead If \code{FALSE}, turn off readahead. Most operating
##'   systems perform readahead on read requests by default. This
##'   option turns it off if the OS supports it. Turning it off may
##'   help random read performance when the DB is larger than RAM and
##'   system RAM is full.  \code{rdahead = FALSE} is not implemented
##'   on Windows.  Passing \code{rdahead = FALSE} is equivalent to
##'   lmdb's \code{MDB_NORDAHEAD} flag.
##'
##' @param meminit If \code{FALSE}, don't initialize malloc'd memory
##'   before writing to unused spaces in the data file. By default,
##'   memory for pages written to the data file is obtained using
##'   malloc. While these pages may be reused in subsequent
##'   transactions, freshly malloc'd pages will be initialized to
##'   zeroes before use. This avoids persisting leftover data from
##'   other code (that used the heap and subsequently freed the
##'   memory) into the data file. Note that many other system
##'   libraries may allocate and free memory from the heap for
##'   arbitrary uses. E.g., stdio may use the heap for file I/O
##'   buffers. This initialization step has a modest performance cost
##'   so some applications may want to disable it using this
##'   flag. This option can be a problem for applications which handle
##'   sensitive data like passwords, and it makes memory checkers like
##'   Valgrind noisy. This flag is not needed with \code{writemap =
##'   TRUE}, which writes directly to the mmap instead of using malloc
##'   for pages.  Passing \code{meminit = FALSE} is equivalent to
##'   lmdb's \code{MDB_NOMEMINIT}.
##'
##' @param maxdbs The number of databases available within the
##'   environment.  If 0 (the default), then the environment holds
##'   just one database (the main dbb).  To use named databases this
##'   must be set greater than one.
##'
##' @param maxreaders Maximum number of simultaneous read
##'   transactions.  Can only be set in the first process to open an
##'   environment.
##'
##' @param mapsize Maximum size database may grow to; used to size the
##'   memory mapping.  If database grows larger than ``map_size``, an
##'   error will be thrown and the user must close and reopen the
##'   \code{mdb_env}.  On 64-bit there is no penalty for making this
##'   huge (say 1TB). Must be <2GB on 32-bit.
##'
##' @param reversekey Passed through to \code{open_database} for the
##'   main database.  If \code{TRUE}, keys are strings to be compared
##'   in reverse order, from the end of the strings to the beginning
##'   (e.g., DNS names). By default, keys are treated as strings and
##'   compared from beginning to end.  Passing \code{reversekey = TRUE}
##'   is equivalent to lmdb's \code{MDB_REVERSEKEY}.
##'
##' @param dupsort Passed through to \code{open_database} for the main
##'   database.  If \code{TRUE}, duplicate keys may be used in the
##'   database. (Or, from another perspective, keys may have multiple
##'   data items, stored in sorted order.) By default keys must be
##'   unique and may have only a single data item.  Passing
##'   \code{dupsort = TRUE} is equivalent to lmdb's
##'   \code{MDB_DUPSORT}.
##'
##' @param create If \code{FALSE}, do not create the directory
##'   \code{path} if it is missing.
##'
##' @template mdb_env
##'
##' @export
##' @examples
##' # Create a new environment (just using defaults)
##' env <- thor::mdb_env(tempfile())
##'
##' # At its most simple (using temporary transactions)
##' env$put("a", "hello world")
##' env$get("a")
##'
##' # Or create transactions
##' txn <- env$begin(write = TRUE)
##' txn$put("b", "another")
##' txn$put("c", "value")
##'
##' # Transaction not committed so value not visible outside our transaction
##' env$get("b", missing_is_error = FALSE)
##'
##' # After committing, the values are visible for new transactions
##' txn$commit()
##' env$get("b", missing_is_error = FALSE)
##'
##' # A convenience method, 'with_transaction' exists to allow
##' # transactional workflows with less code repetition.
##'
##' # This will get the old value of a key 'a', set 'a' to a new value
##' # and return the old value:
##' env$with_transaction(function(txn) {
##'   val <- txn$get("a")
##'   txn$put("a", "new_value")
##'   val
##' }, write = TRUE)
##'
##' # If an error occured, the transaction would be aborted.  So far,
##' # not very interesting!
##'
##' # More interesting: implementing redis's RPOPLPUSH that takes the
##' # last value off of the end of one list and pushes it into the
##' # start of another.
##' rpoplpush <- function(env, src, dest) {
##'   f <- function(txn) {
##'     # Take the value out of the source list and update
##'     val <- unserialize(txn$get(src, as_raw = TRUE))
##'     take <- val[[length(val)]]
##'     txn$put(src, serialize(val[-length(val)], NULL))
##'
##'     # Put the value onto the destination list
##'     val <- unserialize(txn$get(dest, as_raw = TRUE))
##'     txn$put(dest, serialize(c(val, take), NULL))
##'
##'     # And we'll return the value that was modified
##'     take
##'   }
##'   env$with_transaction(f, write = TRUE)
##' }
##'
##' # Set things up - a source list with numbers 1:5 and an empty
##' # destination list
##' env$put("src", serialize(1:5, NULL))
##' env$put("dest", serialize(integer(0), NULL))
##'
##' # then try it out:
##' rpoplpush(env, "src", "dest") # 5
##' rpoplpush(env, "src", "dest") # 4
##' rpoplpush(env, "src", "dest") # 3
##'
##' # Here is the state of the two lists
##' unserialize(env$get("src"))
##' unserialize(env$get("dest"))
##'
##' # The above code will fail if one of the lists is available
##' env$del("dest")
##' try(rpoplpush(env, "src", "dest"))
##'
##' # but because it's in a transaction, this failed attempt leaves src
##' # unchanged
##' unserialize(env$get("src"))
mdb_env <- function(path, mode = as.octmode("644"),
                    subdir = TRUE, readonly = FALSE, metasync = TRUE,
                    sync = TRUE, writemap = FALSE, lock = TRUE,
                    mapasync = FALSE, rdahead = TRUE, meminit = TRUE,
                    ## other args
                    maxdbs = NULL, maxreaders = NULL, mapsize = NULL,
                    reversekey = FALSE, dupsort = FALSE, create = TRUE) {
  R6_mdb_env$new(path, mode,
                 ## flags:
                 subdir = subdir, readonly = readonly, metasync = metasync,
                 sync = sync, writemap = writemap, lock = lock,
                 mapasync = mapasync, rdahead = rdahead,
                 meminit = meminit,
                 ## other:
                 maxdbs = maxdbs, maxreaders = maxreaders, mapsize = mapsize,
                 reversekey = reversekey, dupsort = dupsort, create = create)
}

R6_mdb_env <- R6::R6Class(
  "mdb_env",
  cloneable = FALSE,
  public = list(
    .ptr = NULL,
    .db = NULL,
    .dbs = NULL,
    .deps = NULL,
    .write_txn = NULL,
    .spare_txns = NULL,
    .path = NULL,
    .methods = list(
      Informational = c("path", "flags", "info", "stat",
                        "maxkeysize", "maxreaders"),
      Transactions = c("begin", "with_transaction"),
      Databases = c("open_database", "drop_database"),
      Management = c("sync", "copy", "close", "destroy",
                     "reader_list", "reader_check"),
      Helpers = c("get", "put", "del", "exists", "list",
                  "mget", "mput", "mdel")),

    initialize = function(path, mode,
                          subdir, sync, readonly,
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
                   subdir, sync, readonly,
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

    .new_txn_ptr = function(write, parent_ptr, sync = NULL, metasync = NULL,
                            temporary = FALSE) {
      if (write) {
        self$.check_write()
        ptr <- mdb_txn_begin(self$.ptr, parent_ptr, FALSE, sync, metasync)
        if (!temporary) {
          write_txns[[self$.path]] <- self$.ptr
          self$.write_txn <- ptr
        }
      } else {
        ptr <- self$.spare_txns$pop()
        if (is.null(ptr)) {
          ptr <- mdb_txn_begin(self$.ptr, parent_ptr, TRUE, sync, metasync)
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

    reader_list = function() {
      mdb_reader_list(self$.ptr)
    },
    reader_check = function() {
      mdb_reader_check(self$.ptr)
    },

    copy = function(path, compact = FALSE) {
      assert_scalar_character(path)
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
        R6_mdb_dbi$new(self, txn_ptr, key, reversekey, dupsort, create)
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
      assert_is(db, "mdb_dbi")
      name <- db$.name
      if (is.null(name)) {
        stop("Can't delete root database")
      }
      if (!identical(self$.dbs[[name]], db)) {
        stop("this is not our database")
      }

      dropdb <- function(txn_ptr) {
        for (x in self$.deps$get()) {
          if (inherits(x, "mdb_txn") && identical(x$.db, db)) {
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

    begin = function(db = NULL, write = FALSE, sync = NULL, metasync = NULL) {
      R6_mdb_txn$new(self, db, write, sync, metasync)
    },

    with_transaction = function(fun, db = NULL, write = FALSE) {
      txn <- self$begin(db = db, write = write)
      end <- if (write) txn$commit else txn$abort
      withCallingHandlers({
        ret <- fun(txn)
        end()
        ret
      }, error = function(e) txn$abort())
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
    }
  ))
