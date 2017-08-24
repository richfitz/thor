context("dbi")

test_that("id", {
  env <- mdb_env(tempfile(), maxdbs = 10)

  db <- env$open_database()
  expect_identical(db$id(), 1L)
  expect_null(db$name())

  db2 <- env$open_database("foo")
  expect_identical(db2$id(), 2L)
  expect_identical(db2$name(), "foo")
})

test_that("flags", {
  env <- mdb_env(tempfile(), maxdbs = 10)
  txn <- env$begin()
  db <- env$open_database()
  flags <- db$flags(txn)

  expect_is(flags, "logical")
  expect_true(all(names(flags) %in% names(formals(env$open_database))))
  expect_equal(as.list(formals(env$open_database)[names(flags)]),
               as.list(flags))
})

test_that("format", {
  env <- mdb_env(tempfile())
  dbi <- env$open_database()
  str <- format(dbi)
  expect_false(grepl("initialze", str))
  expect_true(grepl("<mdb_dbi>", str, fixed = TRUE))
  expect_true(grepl("id", str, fixed = TRUE))
})
