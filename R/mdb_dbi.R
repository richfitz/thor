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
    }
  ))
