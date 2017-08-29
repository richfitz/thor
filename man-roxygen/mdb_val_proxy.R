##' @section Methods:
##'
##' \describe{
##' \item{\code{data}}{
##'   Return the value from a proxy object
##'
##'   \emph{Usage:}
##'   \code{data(as_raw = NULL)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{as_raw}:   Return the value as a raw vector?  This has the same semantics as \code{mdb_env$get} - if \code{NULL} then the value will be returned as a string as possible, otherwise as a raw vector.  If \code{TRUE} then the value is always returned as a raw vector, and if \code{FALSE} then the value is always returned as a string (or an error is thrown if that is not possible).
##'     }
##'   }
##'
##'   \emph{Value}:
##'   A string or raw vector
##' }
##' \item{\code{head}}{
##'   Read the first \code{n} bytes from a proxy
##'
##'   \emph{Usage:}
##'   \code{head(n = 6L, as_raw = NULL)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{n}:   The number of bytes to read.  If \code{n} is greater than the length of the object the whole object is returned (same behaviour as \code{\link{head}}
##'     }
##'
##'     \item{\code{as_raw}:   As for \code{$data()}
##'     }
##'   }
##' }
##' \item{\code{is_raw}}{
##'   Return whether we know a value to be raw or not.  This is affected by whether we have successfully turned the value into a string (in which case we can return \code{FALSE}) or if any \code{NULL} bytes have been detected.  The latter condition may be satisfied by reading the first bit of the proxy with \code{$head()}
##'
##'   \emph{Usage:}
##'   \code{is_raw()}
##'
##'   \emph{Value}:
##'   A logical if we can, otherwise \code{NULL} (for symmetry with \code{as_raw})
##' }
##' \item{\code{is_valid}}{
##'   Test if a proxy object is still valid.  Once the proxy is invalid, it cannot be read from any more.  Proxies are invalidated if their parent transaction is closed, or if any write operations (e.g., `put`, `del`) have occured.
##'
##'   \emph{Usage:}
##'   \code{is_valid()}
##'
##'   \emph{Value}:
##'   Scalar logical
##' }
##' \item{\code{size}}{
##'   The size of the data - the number of characters in the string, or number of bytes in the raw vector.
##'
##'   \emph{Usage:}
##'   \code{size()}
##'
##'   \emph{Value}:
##'   Scalar integer
##' }
##' }
