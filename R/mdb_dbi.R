##' Database handles are fairly opaque objects used to indicate which
##' database within an \code{\link{mdb_env}} operations will happen
##' to.  This object has therefore got very few methods, all of which
##' are purely informative.  Most commonly, a \code{mdb_dbi} object
##' will be passed into the \code{\link{mdb_env}}'s \code{$begin()}
##' method to begin a transaction on a particular database.
##'
##' @template mdb_env
##'
##' @title Use mdb transactions
##' @rdname mdb_dbi
##' @aliases mdb_dbi
##' @name mdb_dbi
##' @examples
##'
##' # As always, start with the environment.  Because we're going to
##' # use more than one database, we must set `maxdbs` to more than 1:
##' env <- thor::mdb_env(tempfile(), maxdbs = 10)
##'
##' # The default environment - every database
##' db <- env$open_database()
##' # The default database will always have id 1 and no name
##' db$id()
##' db$name()
##'
##' # A different database
##' foo <- env$open_database("foo")
##' foo$id()
##' foo$name()
##'
##' # Opening a database multiple times has no effect - it returns the
##' # same data base every call.
##' identical(env$open_database("foo"), foo) # TRUE
##'
##' # Then we can put some data int the new database:
##' txn <- env$begin(foo, write = TRUE)
##' txn$put("hello", "world")
##' txn$commit()
##'
##' # Now we have values in the "foo" database, but not the default one:
##' env$get("hello", db = NULL, missing_is_error = FALSE) # NULL
##' env$get("hello", db = foo,  missing_is_error = FALSE) # "world"
##'
##' # Cleanup
##' env$destroy()
NULL

R6_mdb_dbi <- R6::R6Class(
  "mdb_dbi",
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
      assert_is(txn, "mdb_txn")
      mdb_dbi_flags(txn$.ptr, self$.ptr)
    },
    name = function() {
      self$.name
    }
  ))
