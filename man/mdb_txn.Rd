% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mdb_txn.R
\name{mdb_txn}
\alias{mdb_txn}
\title{Use mdb transactions}
\description{
Transactions are required for every mdb operation.  Even when
using the convenience functions in \code{\link{mdb_env}}
(\code{get}, etc), a transaction is created and committed each
time.  Within a transaction, either everything happens or nothing
happens, and everything gets a single consistent view of the
database.
}
\details{
There can be many read transactions per environment, but only one
write transactions.  Because R is single-threaded, that means that
you can only simultaneously write from an mdb environment from a
single object - any further attempts to open write transactions it
would block forever while waiting for a lock that can't be
released because there is only one thread!
}
\section{Methods}{


\describe{
\item{\code{id}}{
  Return the mdb internal id of the transaction

  \emph{Usage:}
  \code{id()}

  \emph{Value}:
  An integer

  \emph{Note}: In lmdb.h this is \code{mdb_txn_id()}
}
\item{\code{stat}}{
  Brief statistics about the database.  This is the same as \code{\link{mdb_env}}'s \code{stat()} but applying to the transaction

  \emph{Usage:}
  \code{stat()}

  \emph{Value}:
  An integer vector with elements \code{psize} (the size of a database page), \code{depth} (depth of the B-tree), \code{brancb_pages} (number of internal non-leaf) pages), \code{leaf_pages} (number of leaf pages), \code{overflow_pages} (number of overflow pages) and \code{entries} (number of data items).

  \emph{Note}: In lmdb.h this is \code{mdb_stat()}
}
\item{\code{commit}}{
  Commit all changes made in this transaction to the database, and invalidate the transaction, and any cursors belonging to it (i.e., once committed the transaction cannot be used again)

  \emph{Usage:}
  \code{commit()}

  \emph{Value}:
  Nothing, called for its side effects only

  \emph{Note}: In lmdb.h this is \code{mdb_txn_commit()}
}
\item{\code{abort}}{
  Abandon all changes made in this transaction to the database, and invalidate the transaction, and any cursors belonging to it (i.e., once aborted the transaction cannot be used again).  For read-only transactions there is no practical difference between abort and commit, except that using \code{abort} allows the transaction to be recycled more efficiently.

  \emph{Usage:}
  \code{abort(cache = TRUE)}

  \emph{Arguments:}
  \itemize{
    \item{\code{cache}:   Logical, indicating if a read-only transaction should be cached for recycling
    }
  }

  \emph{Value}:
  Nothing, called for its side effects only

  \emph{Note}: In lmdb.h this is \code{mdb_txn_abort()}
}
\item{\code{cursor}}{
  Create a \code{\link{mdb_cursor}} object in this transaction. This can be used for more powerful database interactions.

  \emph{Usage:}
  \code{cursor()}

  \emph{Value}:
  A \code{\link{mdb_cursor}} object.

  \emph{Note}: In lmdb.h this is \code{mdb_cursor_open()}
}
\item{\code{get}}{
  Retrieve a value from the database

  \emph{Usage:}
  \code{get(key, missing_is_error = TRUE, as_proxy = FALSE, as_raw = NULL)}

  \emph{Arguments:}
  \itemize{
    \item{\code{key}:   A string (or raw vector) - the key to get
    }

    \item{\code{missing_is_error}:   Logical, indicating if a missing value is an error (by default it is).  Alternatively, with \code{missing_is_error = FALSE}, a missing value will return \code{NULL}.  Because no value can be \code{NULL} (all values must have nonzero length) a \code{NULL} is unambiguously missing.
    }

    \item{\code{as_proxy}:   Return a "proxy" object, which defers doing a copy into R.  See \code{\link{mdb_proxy}} for more information.
    }

    \item{\code{as_raw}:   Either \code{NULL}, or a logical, to indicate the result type required.  With \code{as_raw = NULL}, the default, the value will be returned as a string if possible.  If not possible it will return a raw vector.  With \code{as_raw = TRUE}, \code{get()} will \emph{always} return a raw vector, even when it is possibly to represent the value as a string.  If \code{as_raw = FALSE}, \code{get} will return a string, but throw an error if this is not possible.  This is discussed in more detail in the thor vignette (\code{vignette("thor")})
    }
  }

  \emph{Note}: In lmdb.h this is \code{mdb_get()}
}
\item{\code{put}}{
  Put values into the database.  In other systems, this might be called "\code{set}".

  \emph{Usage:}
  \code{put(key, value, overwrite = TRUE, append = FALSE)}

  \emph{Arguments:}
  \itemize{
    \item{\code{key}:   The name of the key (string or raw vector)
    }

    \item{\code{value}:   The value to save (string or raw vector)
    }

    \item{\code{overwrite}:   Logical - when \code{TRUE} it will overwrite existing data; when \code{FALSE} throw an error
    }

    \item{\code{append}:   Logical - when \code{TRUE}, append the given key/value to the end of the database.  This option allows fast bulk loading when keys are already known to be in the correct order.  But if you load unsorted keys with \code{append = TRUE} an error will be thrown
    }
  }

  \emph{Note}: In lmdb.h this is \code{mdb_put()}
}
\item{\code{del}}{
  Remove a key/value pair from the database

  \emph{Usage:}
  \code{del(key)}

  \emph{Arguments:}
  \itemize{
    \item{\code{key}:   The name of the key (string or raw vector)
    }
  }

  \emph{Value}:
  A scalar logical, indicating if the value was deleted

  \emph{Note}: In lmdb.h this is \code{mdb_del()}
}
\item{\code{exists}}{
  Test if a key exists in the database.

  \emph{Usage:}
  \code{exists(key)}

  \emph{Arguments:}
  \itemize{
    \item{\code{key}:   The name of the key to test (string or raw vector).  Unlike \code{get}, \code{put} and \code{del} (but like \code{mget}, \code{mput} and \code{mdel}), \code{exists} is \emph{vectorised}.  So the input here can be; a character vector of any length (returning the same length logical vector), a raw vector (representing one key, returning a scalar logical) or a \code{list} with each element being either a scalar character or a raw vector, returning a logical the same length as the list.
    }
  }

  \emph{Details:}
  This is an extension of the raw LMDB API and works by using \code{mdb_get} for each key (which for lmdb need not copy data) and then testing whether the return value is \code{MDB_SUCCESS} or \code{MDB_NOTFOUND}.

  \emph{Value}:
  A logical vector
}
\item{\code{list}}{
  List keys in the database

  \emph{Usage:}
  \code{list(starts_with = NULL, as_raw = FALSE, size = NULL)}

  \emph{Arguments:}
  \itemize{
    \item{\code{starts_with}:   Optionally, a prefix for all strings.  Note that is not a regular expression or a filename glob.  Using \code{foo} will match \code{foo}, \code{foo:bar} and \code{foobar} but not \code{fo} or \code{FOO}.  Because LMDB stores keys in a sorted tree, using a prefix can greatly reduce the number of keys that need to be tested.
    }

    \item{\code{as_raw}:   Same interpretation as \code{as_raw} in \code{$get()} but with a different default.  It is expected that most of the time keys will be strings, so by default we'll try and return a character vector \code{as_raw = FALSE}.  Change the default if your database contains raw keys.
    }

    \item{\code{size}:   For use with \code{starts_with}, optionally a guess at the number of keys that would be returned.  with \code{starts_with = NULL} we can look the number of keys up directly so this is ignored.
    }
  }
}
\item{\code{mget}}{
  Get values for multiple keys at once (like \code{$get} but vectorised over \code{key})

  \emph{Usage:}
  \code{mget(key, as_proxy = FALSE, as_raw = NULL)}

  \emph{Arguments:}
  \itemize{
    \item{\code{key}:   The keys to get values for.  Zero, one or more keys are allowed.
    }

    \item{\code{as_proxy}:   Logical, indicating if a list of \code{\link{mdb_proxy}} objects should be returned.
    }

    \item{\code{as_raw}:   As for \code{$get()}, logical (or \code{NULL}) indicating if raw or string output is expected or desired.
    }
  }
}
\item{\code{mput}}{
  Put multiple values into the database (like \code{$put} but vectorised over \code{key}/\code{value}).

  \emph{Usage:}
  \code{mput(key, value, overwrite = TRUE, append = FALSE)}

  \emph{Arguments:}
  \itemize{
    \item{\code{key}:   The keys to set
    }

    \item{\code{value}:   The values to set against these keys.  Must be the same length as \code{key}.
    }

    \item{\code{overwrite}:   As for \code{$put}
    }

    \item{\code{append}:   As for \code{$put}
    }
  }

  \emph{Details:}
  The implementation simply calls \code{mdb_put} repeatedly (but with a single round of error checking) so duplicate \code{key} entries will result in the last key winning.
}
\item{\code{mdel}}{
  Delete multiple values from the database (like \code{$del} but vectorised over \code{key}).

  \emph{Usage:}
  \code{mdel(key)}

  \emph{Arguments:}
  \itemize{
    \item{\code{key}:   The keys to delete
    }
  }

  \emph{Value}:
  A logical vector, the same length as \code{key}, indicating if each key was deleted.
}
\item{\code{replace}}{
  Use a temporary cursor to replace an item; this function will replace the data held at \code{key} and return the previous value (or \code{NULL} if it doesn't exist).  See \code{\link{mdb_cursor}} for fuller documentation.

  \emph{Usage:}
  \code{replace(key, value, as_raw = NULL)}

  \emph{Arguments:}
  \itemize{
    \item{\code{key}:   The key to replace
    }

    \item{\code{value}:   The new value value to st \code{key} to
    }

    \item{\code{as_raw}:   For the returned value, how should the data be returned?
    }
  }

  \emph{Value}:
  As for \code{$get()}, a single data item as either a string or raw vector.
}
\item{\code{pop}}{
  Use a temporary cursor to "pop" an item; this function will delete an item but return the value that it had as it deletes it.

  \emph{Usage:}
  \code{pop(key, as_raw = NULL)}

  \emph{Arguments:}
  \itemize{
    \item{\code{key}:   The key to pop
    }

    \item{\code{as_raw}:   For the returned value, how should the data be returned?
    }
  }

  \emph{Value}:
  As for \code{$get()}, a single data item as either a string or raw vector.
}
\item{\code{cmp}}{
  Compare two keys for ordering

  \emph{Usage:}
  \code{cmp(a, b)}

  \emph{Arguments:}
  \itemize{
    \item{\code{a}:   A key (string or raw); it need not be in the database
    }

    \item{\code{b}:   A key to compare with b (string or raw)
    }
  }

  \emph{Value}:
  A scalar integer, being -1 (if a < b), 0 (if a == b) or 1 (if a > b).

  \emph{Note}: In lmdb.h this is \code{mdb_cmp()}
}
}
}

\examples{
# Start by creating a new environment, and within that a write
# transaction
env <- thor::mdb_env(tempfile())
txn <- env$begin(write = TRUE)

# With this transaction we can write values and see them as set
txn$put("a", "hello")
txn$get("a")

# But because the transaction is not committed, any new
# transaction will not see the values:
env$get("a", missing_is_error = FALSE) # NULL
txn2 <- env$begin()
txn2$get("a", missing_is_error = FALSE) # NULL

# Once we commit a transaction, *new* transactions will see the
# value
txn$commit()
env$get("a") # "hello"
env$begin()$get("a") # "hello"

# But old transactions retain their consistent view of the database
txn2$get("a", missing_is_error = FALSE)

# Cleanup
env$destroy()
}
