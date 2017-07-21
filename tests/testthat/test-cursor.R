context("cursor")

test_that("create", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)
  cur <- txn$cursor()
  expect_is(cur, "cursor")

  expect_identical(env$.deps$get(), list(env$.db, txn))
  expect_identical(txn$.deps$get(), list(cur))
  expect_null(cur$.deps)

  cur_ptr <- cur$.ptr
  txn_ptr <- txn$.ptr
  env_ptr <- env$.ptr

  env$close()
  expect_null(cur$.ptr)
  expect_null(txn$.ptr)
  expect_null(env$.ptr)

  expect_true(is_null_pointer(cur_ptr))
  expect_true(is_null_pointer(txn_ptr))
  expect_true(is_null_pointer(env_ptr))
})

test_that("basic use", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)

  for (i in letters) {
    txn$put(i, toupper(i))
  }

  cur <- txn$cursor()
  expect_null(cur$key())
  expect_null(cur$key(TRUE)$value())

  expect_true(cur$first())
  expect_identical(cur$key(), "a")
  expect_identical(cur$value(), "A")

  cur$put("foo", "bar")
  expect_identical(txn$get("foo"), "bar")

  expect_identical(cur$key(), "foo")
  expect_identical(cur$value(), "bar")
  expect_true(cur$move_next())
  expect_identical(cur$key(), "g")

  mdb_cursor_get(cur$.ptr, cursor_op$FIRST, NULL)
  mdb_cursor_get(cur$.ptr, cursor_op$GET_CURRENT, NULL)

  env$close()
})
