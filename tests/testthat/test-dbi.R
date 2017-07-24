context("dbi")

test_that("id", {
  env <- dbenv(tempfile(), maxdbs = 10)

  db <- env$open_database()
  expect_identical(db$id(), 1L)

  db2 <- env$open_database("foo")
  expect_identical(db2$id(), 2L)
})

test_that("flags", {
  env <- dbenv(tempfile(), maxdbs = 10)
  txn <- env$begin()
  db <- env$open_database()
  flags <- db$flags(txn)

  expect_is(flags, "logical")
  expect_true(all(names(flags) %in% names(formals(env$open_database))))
  expect_equal(as.list(formals(env$open_database)[names(flags)]),
               as.list(flags))
})
