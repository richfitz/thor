context("proxy")

test_that("NULL proxy", {
  txn <- list(.ptr = TRUE, .mutations = 0L)
  for (as_raw in c(TRUE, FALSE)) {
    p <- mdb_val_proxy(txn, NULL, TRUE)
    expect_is(p, "mdb_val_proxy")
    expect_false(environment(p$value)$to_resolve)
    expect_null(p$value())
    expect_identical(p$size(), 0L)
  }
})
