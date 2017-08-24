##' @section Methods:
##'
##' \describe{
##' \item{\code{id}}{
##'   Return the mdb internal id of the transaction
##'
##'   \emph{Usage:}
##'   \code{id()}
##'
##'   \emph{Value}:
##'   An integer
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_txn_id()}
##' }
##' \item{\code{stat}}{
##'   Brief statistics about the database.  This is the same as \code{\link{mdb_env}}'s \code{stat()} but applying to the transaction
##'
##'   \emph{Usage:}
##'   \code{stat()}
##'
##'   \emph{Value}:
##'   An integer vector with elements \code{psize} (the size of a database page), \code{depth} (depth of the B-tree), \code{brancb_pages} (number of internal non-leaf) pages), \code{leaf_pages} (number of leaf pages), \code{overflow_pages} (number of overflow pages) and \code{entries} (number of data items).
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_stat()}
##' }
##' \item{\code{commit}}{
##'   Commit all changes made in this transaction to the database, and invalidate the transaction, and any cursors belonging to it (i.e., once committed the transaction cannot be used again)
##'
##'   \emph{Usage:}
##'   \code{commit()}
##'
##'   \emph{Value}:
##'   Nothing, called for its side effects only
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_txn_commit()}
##' }
##' \item{\code{abort}}{
##'   Abandon all changes made in this transaction to the database, and invalidate the transaction, and any cursors belonging to it (i.e., once aborted the transaction cannot be used again).  For read-only transactions there is no practial difference between abort and commit, except that using \code{abort} allows the transaction to be recycled more efficiently.
##'
##'   \emph{Usage:}
##'   \code{abort(cache = TRUE)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{cache}:   Logical, indicating if a read-only transaction should be cached for recycling
##'     }
##'   }
##'
##'   \emph{Value}:
##'   Nothing, called for its side effects only
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_txn_abort()}
##' }
##' \item{\code{get}}{
##'   Retrieve a value from the database
##'
##'   \emph{Usage:}
##'   \code{get(key, missing_is_error = TRUE, as_proxy = FALSE, as_raw = NULL)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   A string (or raw vector) - the key to get
##'     }
##'
##'     \item{\code{missing_is_error}:   Logical, indicating if a missing value is an error (by default it is).  Alternatively, with \code{missing_is_error = FALSE}, a missing value will return \code{NULL}.  Because no value can be \code{NULL} (all values must have nonzero length) a \code{NULL} is unambiguously missing.
##'     }
##'
##'     \item{\code{as_proxy}:   Return a "proxy" object, which defers doing a copy into R.  See \code{\link{mdb_proxy}} for more information.
##'     }
##'
##'     \item{\code{as_raw}:   Either \code{NULL}, or a logical, to indicate the result type required.  With \code{as_raw = NULL}, the default, the value will be returned as a string if possible.  If not possible it will return a raw vector.  With \code{as_raw = TRUE}, \code{get()} will \emph{always} return a raw vector, even when it is possibly to represent the value as a string.  If \code{as_raw = FALSE}, \code{get} will return a string, but throw an error if this is not possible.  This is discussed in more detail in the thor vignette (\code{vignette("thor")})
##'     }
##'   }
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_get()}
##' }
##' \item{\code{put}}{
##'   Put values into the database.  In other systems, this might be called "\code{set}".
##'
##'   \emph{Usage:}
##'   \code{put(key, value, dupdata = TRUE, overwrite = TRUE, append = FALSE)}
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
##'   }
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_put()}
##' }
##' \item{\code{del}}{
##'   Remove a key/value pair from the database
##'
##'   \emph{Usage:}
##'   \code{del(key, value = NULL)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   The name of the key (string or raw vector)
##'     }
##'
##'     \item{\code{value}:   Optionally, the value of the key - if specified, and if the database was opened with \code{dupsort = TRUE}, only the value matching \code{value} will be deleted.
##'     }
##'   }
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_del()}
##' }
##' }
