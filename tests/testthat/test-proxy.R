context("proxy")

test_that("NULL proxy", {
  txn <- list(.ptr = TRUE, .mutations = 0L)
  p <- mdb_val_proxy(txn, NULL)
  expect_is(p, "mdb_val_proxy")
  expect_null(p$data())
  expect_null(p$data(TRUE))
  expect_null(p$data(FALSE))
  expect_null(p$data(NULL))
  expect_identical(p$size(), 0L)

  environment(p$size)$txn$.mutations <- 1L
  expect_error(p$data(),
               "mdb_val_proxy is invalid: transaction has modified database")
  expect_error(p$size(),
               "mdb_val_proxy is invalid: transaction has modified database")

  environment(p$size)$txn$.ptr <- NULL
  expect_error(p$data(),
               "mdb_val_proxy is invalid: transaction has been closed")
  expect_error(p$size(),
               "mdb_val_proxy is invalid: transaction has been closed")
})
