context("env")

test_that("create & close", {
  path <- tempfile()
  env <- dbenv(path)

  expect_true(file.exists(path))
  expect_true(file.info(path)$isdir)

  expect_is(env, "dbenv")
  expect_is(env, "R6")
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

  ## This test will get reused when we test setting flags and it is
  ## also going to be useful when we replace all the "no" flags.
  flags <- env$flags()
  expect_is(flags, "logical")
  expect_true(all(names(flags) %in% names(formals(dbenv))))
  expect_equal(as.list(formals(dbenv)[names(flags)]),
               as.list(flags))
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

test_that("list readers", {
  env <- dbenv(tempfile())
  cols <- c("pid", "thread", "txnid")
  expect_equal(env$readers(),
               matrix("", 0, 3, dimnames = list(NULL, cols)))
  t1 <- env$begin()
  t2 <- env$begin()
  m <- env$readers()
  expect_is(m, "matrix")
  expect_equal(colnames(m), cols)
  expect_equal(nrow(m), 2L)
  expect_equal(m[, "txnid"], as.character(c(t1$id(), t2$id())))
  expect_equal(m[, "pid"], rep(as.character(Sys.getpid()), 2))
  expect_match(m[, "thread"], "^[[:xdigit:]]+$")
})

test_that("subdir = FALSE", {
  base <- new_empty_dir()
  path <- tempfile(tmpdir = new_empty_dir())
  env <- dbenv(path, subdir = FALSE)

  expect_true(file.exists(path))
  expect_false(file.info(path)$isdir)
  expect_true(file.exists(paste0(path, "-lock")))

  expect_false(env$flags()[["subdir"]])
})
