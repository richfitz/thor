context("utils")

test_that("stack", {
  x <- stack()
  expect_identical(x$get(), list())
  expect_identical(x$length(), 0L)
  x$add(1)
  expect_identical(x$get(), list(1))
  expect_identical(x$length(), 1L)
  x$add(1)
  expect_identical(x$get(), list(1))
  x$push(1)
  expect_identical(x$get(), list(1, 1))
  x$discard(1)
  expect_identical(x$get(), list(1))
  expect_identical(x$pop(), 1)
  expect_identical(x$get(), list())
  expect_null(x$pop())

  x$add(1)
  x$add(2)
  expect_identical(x$length(), 2L)
  x$clear()
  expect_identical(x$length(), 0L)
})

test_that("list_to_fixed_env", {
  e <- list_to_fixed_env(setNames(seq_len(4), letters[1:4]),
                         new.env(parent = emptyenv()))
  expect_is(e, "environment")
  expect_identical(e[["a"]], 1L)
  expect_identical(e[["d"]], 4L)
  expect_true(environmentIsLocked(e))
  expect_true(bindingIsLocked("a", e))
  expect_true(bindingIsLocked("d", e))
  expect_error(e$a <- 1)
  expect_error(e$d <- 1)
})

test_that("assert_is", {
  x <- 1L
  expect_silent(assert_is(x, "integer"))
  expect_error(assert_is(x, "character"), "'x' must be a character")
  expect_error(assert_is(x, c("foo", "bar")),
               "'x' must be a foo / bar")
})

test_that("as_integer", {
  expect_identical(as_integer(1L), 1L)
  expect_identical(as_integer(1.0), 1L)
  expect_error(as_integer(pi), "'pi' must be an integer", fixed = TRUE)
  expect_error(as_integer(c(.1, .2)), "must be a scalar", fixed = TRUE)
})

test_that("scalar_character (C)", {
  expect_error(mdb_env(character(0), create = FALSE),
               "Expected a scalar character for 'path'")
  expect_error(mdb_env(letters, create = FALSE),
               "Expected a scalar character for 'path'")
  expect_error(mdb_env(NULL, create = FALSE),
               "Expected a scalar character for 'path'")
  expect_error(mdb_env(1L, create = FALSE),
               "Expected a scalar character for 'path'")
})

test_that("scalar_int (C)", {
  expect_error(mdb_env(tempfile(), maxdbs = integer(0)),
               "Expected a scalar integer for 'dbs'")
  expect_error(mdb_env(tempfile(), maxdbs = seq_len(2)),
               "Expected a scalar integer for 'dbs'")
  expect_error(mdb_env(tempfile(), maxdbs = -5L),
               "Expected a positive size for 'dbs'")
})

test_that("scalar_logical (C)", {
  expect_error(mdb_env(tempfile(), subdir = NA, create = FALSE),
               "Expected a non-missing scalar logical for 'subdir'")
  expect_error(mdb_env(tempfile(), subdir = "why not", create = FALSE),
               "Expected a scalar logical for 'subdir'")
})

test_that("to_return_as (C)", {
  env <- mdb_env(tempfile())
  txn <- env$begin()
  expect_error(txn$get("a", as_raw = NA),
               "Expected a non-missing logical scalar (or NULL) for 'as_raw'",
               fixed = TRUE)
  expect_error(txn$get("a", as_raw = 1),
               "Expected a logical scalar (or NULL) for 'as_raw'",
               fixed = TRUE)
})

test_that("to_return_as (C)", {
  env <- mdb_env(tempfile())
  txn <- env$begin()
  expect_error(txn$get(c("a", "b")), "'key' must be a scalar character")
  expect_error(txn$get(character()), "'key' must be a scalar character")
  expect_error(txn$get(1L), "Invalid data type for 'key'")
})

test_that("is_null_pointer", {
  expect_error(is_null_pointer(NULL), "Expected an external pointer", fixed = TRUE)
  env <- mdb_env_create()
  expect_false(is_null_pointer(env))
  expect_true(is_null_pointer(unserialize(serialize(env, NULL))))
})

test_that("error detection", {
  no_error <- function(rc, str) {
    .Call(Ctest_error, rc, NULL, str)
  }
  no_error2 <- function(rc, false_flag, str) {
    .Call(Ctest_error, rc, false_flag, str)
  }

  SUCCESS <- 0L
  NOTFOUND <- -30798L
  KEYEXIST <- -30799L

  expect_true(no_error(SUCCESS, "foo"))
  expect_error(no_error(KEYEXIST, "foo"),
               "MDB_KEYEXIST: Key/data pair already exists: foo")

  expect_true(no_error2(SUCCESS, NOTFOUND, "foo"))
  expect_false(no_error2(NOTFOUND, NOTFOUND, "foo"))
  expect_error(no_error2(KEYEXIST, NOTFOUND, "foo"),
               "MDB_KEYEXIST: Key/data pair already exists: foo")
})

test_that("pointer_addr_str", {
  env <- mdb_env_create()
  pointer_addr_str(env)
})
