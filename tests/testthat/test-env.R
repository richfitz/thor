context("env")

test_that("create & close", {
  env <- dbenv(tempfile())

  expect_is(env, "dbenv")
  expect_equal(mode(env$.ptr), "externalptr")

  expect_is(env$.db, "database")
  expect_equal(env$.deps$get(), list(env$.db))

  expect_equal(ls(env$.dbs, all.names = TRUE), character(0))
  expect_null(env$.write_txn)

  env$close()

  ## This is the expected state after closing:
  expect_null(env$.ptr)
  expect_null(env$.db)
  expect_null(env$.deps)
  expect_null(env$.dbs)
  expect_null(env$.write_txn)

  ## This is OK
  env$close()

  ## But this will cause an error
  expect_error(env$open_database(),
               "env has been cleaned up; can't use")
})

test_that("information", {
  p <- tempfile()
  env <- dbenv(p)
  expect_true(file.exists(p))
  expect_true(file.info(p)$isdir)

  stat <- env$stat()
  expect_is(stat, "integer")
  expect_equal(names(stat),
               c("psize", "depth", "branch_pages", "leaf_pages",
                 "overflow_pages", "entries"))

  info <- env$info()
  expect_is(info, "integer")
  expect_equal(names(info),
               c("mapsize", "last_pgno", "last_txnid", "maxreaders",
                 "numreaders"))

  expect_identical(env$maxkeysize(), 511L)
  expect_identical(env$maxreaders(), 126L)

  expect_identical(env$path(), p)
})

test_that("no create", {
  p <- tempfile()
  expect_error(dbenv(p, create = FALSE), "No such file or directory")
  expect_false(file.exists(p))

  ## This surprises me a bit:
  dir.create(p)
  env <- dbenv(p, create = FALSE)
  expect_is(env, "dbenv")
})
