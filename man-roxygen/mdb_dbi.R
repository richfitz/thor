##' @section Methods:
##'
##' \describe{
##' \item{\code{id}}{
##'   Return the mdb internal id of the database
##'
##'   \emph{Usage:}
##'   \code{id()}
##'
##'   \emph{Value}:
##'   An integer
##' }
##' \item{\code{name}}{
##'   Return the name of the database
##'
##'   \emph{Usage:}
##'   \code{name()}
##'
##'   \emph{Value}:
##'   A string, or \code{NULL} for the default database, which has no name
##' }
##' \item{\code{flags}}{
##'   Return flags used by the database handle
##'
##'   \emph{Usage:}
##'   \code{flags(txn)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{txn}:   A transaction (can be read-only) to use to read from the database.
##'     }
##'   }
##'
##'   \emph{Value}:
##'   A named logical vector.  Names correspond to arguments to the constructor (\code{reversekey} and \code{dupsort}).
##'
##'   \emph{Note}: In lmdb.h this is \code{mdb_dbi_flags()}
##' }
##' }
