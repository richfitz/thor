##' @section Methods:
##'
##' \describe{
##' \item{\code{close}}{
##'   Close the cursor
##'
##'   \emph{Usage:}
##'   \code{close()}
##'
##'   \emph{Value}:
##'   None, called for side effects only
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_cursor_close()}
##' }
##' \item{\code{put}}{
##'   Store data using the cursor
##'
##'   \emph{Usage:}
##'   \code{put(key, value, overwrite = TRUE, append = FALSE)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   The key (string or raw)
##'     }
##'
##'     \item{\code{value}:   The value (string or raw)
##'     }
##'
##'     \item{\code{overwrite}:   As for \code{\link{mdb_txn}} \code{$put}
##'     }
##'
##'     \item{\code{append}:   As for \code{\link{mdb_txn}} \code{$put}
##'     }
##'   }
##'
##'   \emph{Value}:
##'   Logical scalar, indicating if data was previously stored at this key
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_cursor_put()}
##' }
##' \item{\code{del}}{
##'   Delete the current key
##'
##'   \emph{Usage:}
##'   \code{del()}
##'
##'   \emph{Value}:
##'   Logical, indicating if a value was deleted (which will be \code{TRUE} if the cursor was valid before this operation). Primarily called for its side effect of deleting the data.  After deletion, we call \code{mdb_cursor_get} with \code{MDB_GET_CURRENT} which will re-validate the cursor.
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_cursor_del()}
##' }
##' \item{\code{replace}}{
##'   Replace a key's current value with a new value, returning the old value.  This is like doing a \code{get()} followed by a \code{put} within a transaction.
##'
##'   \emph{Usage:}
##'   \code{replace(key, value, as_raw = NULL)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   The key to replace
##'     }
##'
##'     \item{\code{value}:   The new value to store
##'     }
##'
##'     \item{\code{as_raw}:   Return the value as raw.  With a value of \code{NULL} it will return a string if possible (i.e., if there are no null bytes) and a raw vector otherwise.  With \code{as_raw = TRUE} we always return a raw vector.  With \code{as_raw = FALSE} we always return a string, or throw an error if this is not possible.
##'     }
##'   }
##' }
##' \item{\code{pop}}{
##'   Delete a key's value, returning the value just before it was deleted.  This is like doing a \code{get} followed by a \code{del} within a transaction.
##'
##'   \emph{Usage:}
##'   \code{pop(key, as_raw = NULL)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   The key to delete
##'     }
##'
##'     \item{\code{as_raw}:   Return the value as raw.  With a value of \code{NULL} it will return a string if possible (i.e., if there are no null bytes) and a raw vector otherwise.  With \code{as_raw = TRUE} we always return a raw vector.  With \code{as_raw = FALSE} we always return a string, or throw an error if this is not possible.
##'     }
##'   }
##'
##'   \emph{Value}:
##'   Depending on \code{as_raw} and if there is a value stored, \code{NULL}, a character string or a raw vector
##' }
##' \item{\code{first}}{
##'   Move the cursor to the first item in the database
##'
##'   \emph{Usage:}
##'   \code{first()}
##'
##'   \emph{Value}:
##'   Invisibly, a logical indicating if the cursor position is valid, but primarily called for side effects
##' }
##' \item{\code{last}}{
##'   Move the cursor to the last item in the database
##'
##'   \emph{Usage:}
##'   \code{last()}
##'
##'   \emph{Value}:
##'   Invisibly, a logical indicating if the cursor position is valid, but primarily called for side effects
##' }
##' \item{\code{move_next}}{
##'   Move the cursor to the next item in the database.  If called while at the last item in the database, this will invalidate the cursor position.
##'
##'   \emph{Usage:}
##'   \code{move_next()}
##'
##'   \emph{Value}:
##'   Invisibly, a logical indicating if the cursor position is valid, but primarily called for side effects
##' }
##' \item{\code{move_prev}}{
##'   Move the cursor to the previous item in the database.  If called while at the first item in the database, this will invalidate the cursor position.
##'
##'   \emph{Usage:}
##'   \code{move_prev()}
##'
##'   \emph{Value}:
##'   Invisibly, a logical indicating if the cursor position is valid, but primarily called for side effects
##' }
##' \item{\code{move_to}}{
##'   Move the cursor to the item in the database with key \code{key}. If \code{key} does not exist, this will invalidate the cursor position.
##'
##'   \emph{Usage:}
##'   \code{move_to(key)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   Key to move to (string or raw)
##'     }
##'   }
##'
##'   \emph{Value}:
##'   Invisibly, a logical indicating if the cursor position is valid, but primarily called for side effects
##' }
##' \item{\code{seek}}{
##'   Move the cursor to the item in the database with key equal to or greater than \code{key}.  If \code{key} does not exist and no key with a key greater than \code{key} exists, this will invalidate the cursor position.
##'
##'   \emph{Usage:}
##'   \code{seek(key)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   Key to seek (string or raw)
##'     }
##'   }
##'
##'   \emph{Value}:
##'   Invisibly, a logical indicating if the cursor position is valid, but primarily called for side effects
##' }
##' \item{\code{get}}{
##'   Move to a key and fetch the value
##'
##'   \emph{Usage:}
##'   \code{get(key, as_proxy = FALSE, as_raw = NULL)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   The key to find (string or raw)
##'     }
##'
##'     \item{\code{as_proxy}:   Return as an \code{\link{mdb_proxy}} object?
##'     }
##'
##'     \item{\code{as_raw}:   Return the value as raw.  With a value of \code{NULL} it will return a string if possible (i.e., if there are no null bytes) and a raw vector otherwise.  With \code{as_raw = TRUE} we always return a raw vector.  With \code{as_raw = FALSE} we always return a string, or throw an error if this is not possible.
##'     }
##'   }
##'
##'   \emph{Value}:
##'   Depending on \code{as_raw} and if there is a value stored, \code{NULL}, a character string or a raw vector
##' }
##' \item{\code{is_valid}}{
##'   Test if cursor is valid (i.e., that it is pointing at data that can be retrieved).  Cursors start off invalid until placed (e.g., \code{first}, \code{last}) and can be invalidated by moving off the beginning or end of the database.
##'
##'   \emph{Usage:}
##'   \code{is_valid()}
##' }
##' \item{\code{key}}{
##'   Return the current key
##'
##'   \emph{Usage:}
##'   \code{key(as_proxy = FALSE, as_raw = NULL)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{as_proxy}:   Return as an \code{\link{mdb_proxy}} object?
##'     }
##'
##'     \item{\code{as_raw}:   Return the value as raw.  With a value of \code{NULL} it will return a string if possible (i.e., if there are no null bytes) and a raw vector otherwise.  With \code{as_raw = TRUE} we always return a raw vector.  With \code{as_raw = FALSE} we always return a string, or throw an error if this is not possible.
##'     }
##'   }
##' }
##' \item{\code{value}}{
##'   Return the current value
##'
##'   \emph{Usage:}
##'   \code{value(as_proxy = FALSE, as_raw = NULL)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{as_proxy}:   Return as an \code{\link{mdb_proxy}} object?
##'     }
##'
##'     \item{\code{as_raw}:   Return the value as raw.  With a value of \code{NULL} it will return a string if possible (i.e., if there are no null bytes) and a raw vector otherwise.  With \code{as_raw = TRUE} we always return a raw vector.  With \code{as_raw = FALSE} we always return a string, or throw an error if this is not possible.
##'     }
##'   }
##' }
##' }
