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
##'     \item{\code{as_raw}:   Return the value as a raw vector?  This has the same semantics as \code{\link{mdb_env$get}} - if \code{NULL} then the value will be returned as a string as possible, otherwise as a raw vector.  If \code{TRUE} then the value is always returned as a raw vector, and if \code{FALSE} then the value is always returned as a string (or an error is thrown if that is not possible).
##'     }
##'   }
##'
##'   \emph{Value}:
##'   A string or raw vector
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
