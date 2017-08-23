##' @section Methods:
##'
##' \describe{
##' \item{\code{path}}{
##'   Print the absolute path to the LMDB store (on disk)
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
##' \item{\code{destroy}}{
##'   Totally destroy an LMDB environment.  This closes the database and removes the files.  Use with care!
##'
##'   \emph{Usage:}
##'   \code{destroy()}
##'
##'   \emph{Value}:
##'   No return value, called for side effects only
##' }
##' }
