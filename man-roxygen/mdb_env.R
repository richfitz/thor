##' @section Methods:
##'
##' \describe{
##' \item{\code{path}}{
##'   Return the absolute path to the LMDB store (on disk)
##'
##'   \emph{Usage:}
##'   \code{path()}
##'
##'   \emph{Value}:
##'   A string
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_env_get_path()}
##' }
##' \item{\code{flags}}{
##'   Return flags as used in construction of the LMDB environment
##'
##'   \emph{Usage:}
##'   \code{flags()}
##'
##'   \emph{Value}:
##'   A named logical vector.  Names correspond to arguments to the constructor.
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_env_get_flags()}
##' }
##' \item{\code{info}}{
##'   Brief information about the LMDB environment
##'
##'   \emph{Usage:}
##'   \code{info()}
##'
##'   \emph{Value}:
##'   An integer vector with elements \code{mapsize}, \code{last_pgno}, \code{last_txnid}, \code{maxreaders} and \code{numreaders}.
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_env_info()}
##' }
##' \item{\code{stat}}{
##'   Brief statistics about the LMDB environment.
##'
##'   \emph{Usage:}
##'   \code{stat()}
##'
##'   \emph{Value}:
##'   An integer vector with elements \code{psize} (the size of a database page), \code{depth} (depth of the B-tree), \code{brancb_pages} (number of internal non-leaf) pages), \code{leaf_pages} (number of leaf pages), \code{overflow_pages} (number of overflow pages) and \code{entries} (number of data items).
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_env_stat()}
##' }
##' \item{\code{maxkeysize}}{
##'   The maximum size of a key (the value can be bigger than this)
##'
##'   \emph{Usage:}
##'   \code{maxkeysize()}
##'
##'   \emph{Value}:
##'   A single integer
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_env_get_maxkeysize()}
##' }
##' \item{\code{maxreaders}}{
##'   The maximum number of readers
##'
##'   \emph{Usage:}
##'   \code{maxreaders()}
##'
##'   \emph{Value}:
##'   A single integer
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_env_get_maxreaders()}
##' }
##' \item{\code{begin}}{
##'   Begin a transaction
##'
##'   \emph{Usage:}
##'   \code{begin(db = NULL, write = FALSE)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{db}:   A database handle, as returned by \code{open_database}.  If \code{NULL} (the default) then the default database will be used.
##'     }
##'
##'     \item{\code{write}:   Scalar logical, indicating if this should be a write transaction.  There can be only one write transaction per database (see \code{\link{mdb_txn}} for more details) - it is an error to try to open more than one.
##'     }
##'   }
##'
##'   \emph{Details:}
##'   Transactions are the key objects for interacting with an LMDB database (aside from the convenience interface below).  They are described in more detail in \code{\link{mdb_txn}}.
##'
##'   \emph{Value}:
##'   A \code{\link{mdb_txn}} object
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_begin()}
##' }
##' \item{\code{with_transaction}}{
##'   Evaluate some code within a transaction
##'
##'   \emph{Usage:}
##'   \code{with_transaction(fun, db = NULL, write = FALSE)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{fun}:   A function of one argument that does the work of the transaction.  \code{with_transaction} will pass the transaction to this function.  This is most eaily explained with an example, so see the bottom of the help
##'     }
##'
##'     \item{\code{db}:   A database handle, as returned by \code{open_database}.  If \code{NULL} (the default) then the default database will be used.
##'     }
##'
##'     \item{\code{write}:   Scalar logical, indicating if this should be a write transaction.  There can be only one write transaction per database (see \code{\link{mdb_txn}} for more details) - it is an error to try to open more than one.
##'     }
##'   }
##'
##'   \emph{Details:}
##'   This exists to simplify a pattern where one wants to open a transaction, evaluate some code with that transaction and if anything goes wrong abort, but otherwise commit.  It is most useful with read-write transactions, but can be used with both (and the default is for readonoly transactions, like \code{begin()}.
##' }
##' \item{\code{open_database}}{
##'   Open a named database, or return one if already opened.
##'
##'   \emph{Usage:}
##'   \code{open_database(key = NULL, reversekey = FALSE, dupsort = FALSE, create = TRUE)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   Name of the database; if \code{NULL} this returns the default database (always open).
##'     }
##'
##'     \item{\code{reversekey}:   Compare strings in reverse order? See \code{reversekey} documentation above
##'     }
##'
##'     \item{\code{dupsort}:   Allow use of duplicate keys in the database?  See \code{dupsort} above.
##'     }
##'
##'     \item{\code{create}:   Create database if it does not exist already?
##'     }
##'   }
##'
##'   \emph{Details:}
##'   LMDB environments can hold multiple databases, provided they have been opened with \code{maxdbs} greater than one.  There is always a "default" database - this is unnamed and cannot be dropped. Other databases have a key (i.e., a name) and can be dropped. These database objects are passed through to other methods, notably \code{drop_database} and \code{begin}
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_open()}
##' }
##' \item{\code{drop_database}}{
##'   Drop a database
##'
##'   \emph{Usage:}
##'   \code{drop_database(db, delete = TRUE)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{db}:   A database object, as returned by \code{open_database}
##'     }
##'
##'     \item{\code{delete}:   Scalar logical, indicating if the database should be deleted too.  If \code{FALSE}, the values are deleted from the database (i.e., it is emptied). If \code{TRUE} then the actual database is deleted too.
##'     }
##'   }
##'
##'   \emph{Value}:
##'   No return value, called for side effects only
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_drop()}
##' }
##' \item{\code{sync}}{
##'   Flush the data buffers to disk.
##'
##'   \emph{Usage:}
##'   \code{sync(force = FALSE)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{force}:   Scalar logical; force a synchronous flush.  Otherwise if the environment was constructed with \code{sync = FALSE} the flushes will be omitted, and with \code{mapasync = TRUE} they will be asynchronous.
##'     }
##'   }
##'
##'   \emph{Details:}
##'   Data is always written to disk when a transaction is committed, but the operating system may keep it buffered.  LMDB always flushes the OS buffers upon commit as well, unless the environment was opened with \code{sync = FALSE} or in part \code{metasync = FALSE}.  This call is not valid if the environment was opened with \code{rdonly = TRUE}.
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_env_sync()}
##' }
##' \item{\code{copy}}{
##'   Copy the entire environment state to a new path.  This can be used to make a backup of the database.
##'
##'   \emph{Usage:}
##'   \code{copy(path, compact = FALSE)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{path}:   Scalar character; the new path
##'     }
##'
##'     \item{\code{compact}:   Scalar logical; perform compaction while copying?  This omits free pages and sequentially renumbers all pages in output.  This can take longer than the default but produce a smaller database
##'     }
##'   }
##'
##'   \emph{Value}:
##'   Invisibly, the new path (allowing use of \code{$copy(tempfile)})
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_env_copy()} & \code{mdb_env_copy2()}
##' }
##' \item{\code{close}}{
##'   Close the environment.  This closes all cursors and transactions (active write transactions are aborted).
##'
##'   \emph{Usage:}
##'   \code{close()}
##'
##'   \emph{Value}:
##'   No return value, called for side effects only
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_env_close()}
##' }
##' \item{\code{destroy}}{
##'   Totally destroy an LMDB environment.  This closes the database and removes the files.  Use with care!
##'
##'   \emph{Usage:}
##'   \code{destroy()}
##'
##'   \emph{Value}:
##'   No return value, called for side effects only
##' }
##' \item{\code{reader_list}}{
##'   List information about database readers
##'
##'   \emph{Usage:}
##'   \code{reader_list()}
##'
##'   \emph{Value}:
##'   A character matrix with columns \code{pid} (process ID), \code{thread} (a pointer address), and \code{txnid} (a small integer)
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_reader_list()}
##' }
##' \item{\code{reader_check}}{
##'   Check for, and remove, stale entries in the reader lock table.
##'
##'   \emph{Usage:}
##'   \code{reader_check()}
##'
##'   \emph{Value}:
##'   An integer, being the number of stale readers discarded.  However, this function is primarily called for its side effect.
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_reader_check()}
##' }
##' \item{\code{get}}{
##'   Retrieve a value from the database
##'
##'   \emph{Usage:}
##'   \code{get(key, missing_is_error = TRUE, as_raw = NULL, db = NULL)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   A string (or raw vector) - the key to get
##'     }
##'
##'     \item{\code{missing_is_error}:   Logical, indicating if a missing value is an error (by default it is).  Alternatively, with \code{missing_is_error = FALSE}, a missing value will return \code{NULL}.  Because no value can be \code{NULL} (all values must have nonzero length) a \code{NULL} is unambiguously missing.
##'     }
##'
##'     \item{\code{as_raw}:   Either \code{NULL}, or a logical, to indicate the result type required.  With \code{as_raw = NULL}, the default, the value will be returned as a string if possible.  If not possible it will return a raw vector.  With \code{as_raw = TRUE}, \code{get()} will \emph{always} return a raw vector, even when it is possibly to represent the value as a string.  If \code{as_raw = FALSE}, \code{get} will return a string, but throw an error if this is not possible.  This is discussed in more detail in the thor vignette (\code{vignette("thor")})
##'     }
##'
##'     \item{\code{db}:   A database handle that would be passed through to create the transaction (see the \code{$begin} method).
##'     }
##'   }
##'
##'   \emph{Details:}
##'   This is a helper method that establishes a temporary read-only transaction, calls the corresponding method in \code{\link{mdb_txn}} and then aborts the transaction.
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_get()}
##' }
##' \item{\code{put}}{
##'   Put values into the database.  In other systems, this might be called "\code{set}".
##'
##'   \emph{Usage:}
##'   \code{put(key, value, dupdata = TRUE, overwrite = TRUE, append = FALSE,
##'       db = NULL)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   The name of the key (string or raw vector)
##'     }
##'
##'     \item{\code{value}:   The value to save (string or raw vector)
##'     }
##'
##'     \item{\code{dupdata}:   if \code{FALSE}, add data only where the key already exists in the database.  This is valid only when the database was opened with \code{dupdata = TRUE}.
##'     }
##'
##'     \item{\code{overwrite}:   Logical - when \code{TRUE} it will overwrite existing data; when \code{FALSE} throw an error
##'     }
##'
##'     \item{\code{append}:   Logical - when \code{TRUE}, append the given key/value to the end of the database.  This option allows fast bulk loading when keys are already known to be in the correct order.  But if you load unsorted keys with \code{append = TRUE} an error will be thrown
##'     }
##'
##'     \item{\code{db}:   A database handle that would be passed through to create the transaction (see the \code{$begin} method).
##'     }
##'   }
##'
##'   \emph{Details:}
##'   This is a helper method that establishes a temporary read-write transaction, calls the correspinding method in \code{\link{mdb_txn}} and then commits the transaction.  This will only be possible to use if there is not an existing write transaction in effect for this environment.
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_put()}
##' }
##' \item{\code{del}}{
##'   Remove a key/value pair from the database
##'
##'   \emph{Usage:}
##'   \code{del(key, value = NULL, db = NULL)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   The name of the key (string or raw vector)
##'     }
##'
##'     \item{\code{value}:   Optionally, the value of the key - if specified, and if the database was opened with \code{dupsort = TRUE}, only the value matching \code{value} will be deleted.
##'     }
##'
##'     \item{\code{db}:   A database handle that would be passed through to create the transaction (see the \code{$begin} method).
##'     }
##'   }
##'
##'   \emph{Details:}
##'   This is a helper method that establishes a temporary read-write transaction, calls the correspinding method in \code{\link{mdb_txn}} and then commits the transaction.  This will only be possible to use if there is not an existing write transaction in effect for this environment.
##'
##'   \emph{Value}:
##'   A scalar logical, indicating if the value was deleted
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_del()}
##' }
##' \item{\code{exists}}{
##'   Test if a key exists in the database.
##'
##'   \emph{Usage:}
##'   \code{exists(key, db = NULL)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   The name of the key to test (string or raw vector).  Unlike \code{get}, \code{put} and \code{del} (but like \code{mget}, \code{mput} and \code{mdel}), \code{exists} is \emph{vectorised}.  So the input here can be; a character vector of any length (returning the same length logical vector), a raw vector (representing one key, returning a scalar logical) or a \code{list} with each element being either a scalar character or a raw vector, returning a logical the same length as the list.
##'     }
##'
##'     \item{\code{db}:   A database handle that would be passed through to create the transaction (see the \code{$begin} method).
##'     }
##'   }
##'
##'   \emph{Details:}
##'   This is an extension of the raw LMDB API and works by using \code{mdb_get} for each key (which for lmdb need not copy data) and then testing whether the return value is \code{MDB_SUCCESS} or \code{MDB_NOTFOUND}.
##'
##'   This is a helper method that establishes a temporary read-only transaction, calls the corresponding method in \code{\link{mdb_txn}} and then aborts the transaction.
##'
##'   \emph{Value}:
##'   A logical vector
##' }
##' \item{\code{list}}{
##'   List keys in the database
##'
##'   \emph{Usage:}
##'   \code{list(starts_with = NULL, as_raw = FALSE, size = NULL, db = NULL)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{starts_with}:   Optionally, a prefix for all strings.  Note that is not a regular expression or a filename glob.  Using \code{foo} will match \code{foo}, \code{foo:bar} and \code{foobar} but not \code{fo} or \code{FOO}.  Because LMDB stores keys in a sorted tree, using a prefix can greatly reduce the number of keys that need to be tested.
##'     }
##'
##'     \item{\code{as_raw}:   Same interpretation as \code{as_raw} in \code{$get()} but with a different default.  It is expected that most of the time keys will be strings, so by default we'll try and return a character vector \code{as_raw = FALSE}.  Change the default if your database contains raw keys.
##'     }
##'
##'     \item{\code{size}:   For use with \code{starts_with}, optionally a guess at the number of keys that would be returned.  with \code{starts_with = NULL} we can look the number of keys up directly so this is ignored.
##'     }
##'
##'     \item{\code{db}:   A database handle that would be passed through to create the transaction (see the \code{$begin} method).
##'     }
##'   }
##'
##'   \emph{Details:}
##'   This is a helper method that establishes a temporary read-only transaction, calls the corresponding method in \code{\link{mdb_txn}} and then aborts the transaction.
##' }
##' \item{\code{mget}}{
##'   Get values for multiple keys at once (like \code{$get} but vectorised over \code{key})
##'
##'   \emph{Usage:}
##'   \code{mget(key, as_raw = NULL, db = NULL)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   The keys to get values for.  Zero, one or more keys are allowed.
##'     }
##'
##'     \item{\code{as_raw}:   As for \code{$get()}, logical (or \code{NULL}) indicating if raw or string output is expected or desired.
##'     }
##'
##'     \item{\code{db}:   A database handle that would be passed through to create the transaction (see the \code{$begin} method).
##'     }
##'   }
##'
##'   \emph{Details:}
##'   This is a helper method that establishes a temporary read-only transaction, calls the corresponding method in \code{\link{mdb_txn}} and then aborts the transaction.
##' }
##' \item{\code{mput}}{
##'   Put multiple values into the database (like \code{$put} but vectorised over \code{key}/\code{value}).
##'
##'   \emph{Usage:}
##'   \code{mput(key, value, dupdata = TRUE, overwrite = TRUE, append = FALSE,
##'       db = NULL)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   The keys to set
##'     }
##'
##'     \item{\code{value}:   The values to set against these keys.  Must be the same length as \code{key}.
##'     }
##'
##'     \item{\code{dupdata}:   As for \code{$put}
##'     }
##'
##'     \item{\code{overwrite}:   As for \code{$put}
##'     }
##'
##'     \item{\code{append}:   As for \code{$put}
##'     }
##'
##'     \item{\code{db}:   A database handle that would be passed through to create the transaction (see the \code{$begin} method).
##'     }
##'   }
##'
##'   \emph{Details:}
##'   The implementation simply calls \code{mdb_put} repeatedly (but with a single round of error checking) so duplicate \code{key} entries will result in the last key winning.
##'
##'   This is a helper method that establishes a temporary read-write transaction, calls the correspinding method in \code{\link{mdb_txn}} and then commits the transaction.  This will only be possible to use if there is not an existing write transaction in effect for this environment.
##' }
##' \item{\code{mdel}}{
##'   Delete multiple values from the database (like \code{$del} but vectorised over \code{key}).
##'
##'   \emph{Usage:}
##'   \code{mdel(key, value = NULL, db = NULL)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   The keys to delete
##'     }
##'
##'     \item{\code{value}:   As for \code{$del}.  If used, must be the same length as \code{key}.
##'     }
##'
##'     \item{\code{db}:   A database handle that would be passed through to create the transaction (see the \code{$begin} method).
##'     }
##'   }
##'
##'   \emph{Details:}
##'   This is a helper method that establishes a temporary read-write transaction, calls the correspinding method in \code{\link{mdb_txn}} and then commits the transaction.  This will only be possible to use if there is not an existing write transaction in effect for this environment.
##'
##'   \emph{Value}:
##'   A logical vector, the same length as \code{key}, indicating if each key was deleted.
##' }
##' }
