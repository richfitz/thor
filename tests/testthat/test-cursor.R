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

test_that("cursor open close", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)
  for (i in letters) {
    txn$put(i, toupper(i))
  }

  cur <- txn$cursor()
  expect_identical(txn$.deps$get(), list(cur))

  expect_identical(cur$get("a"), "A")
  cur$close()
  expect_null(cur$.ptr)
  expect_identical(txn$.deps$get(), list())
  expect_error(cur$get("a"), "cursor has been cleaned up; can't use")
})

test_that("cursor get", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)
  for (i in letters) {
    txn$put(i, toupper(i))
  }

  cur <- txn$cursor()
  cur$last()
  expect_identical(cur$key(), "z")
  p <- cur$key(TRUE)
  expect_is(p, "mdb_val_proxy")
  expect_identical(p$value(), "z")

  ## Then we modify the data
  txn$del("g")

  expect_false(p$is_valid())
  expect_error(p$value(), "transaction has modified database")
  expect_identical(cur$key(), "z")
})

test_that("iteration", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)
  for (i in letters) {
    txn$put(i, toupper(i))
  }

  cur <- txn$cursor()
  cur$first()
  expect_false(cur$move_prev())
  expect_null(cur$key())

  cur$first()
  res <- cur$value()
  while (cur$move_next()) {
    res <- c(res, cur$value())
  }
  expect_identical(res, LETTERS)
})

test_that("move_to", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)
  objects <- c("apple", "banana", "carrot", "dog")
  for (i in objects) {
    txn$put(i, toupper(i))
  }

  cur <- txn$cursor()
  expect_true(cur$move_to("banana"))
  expect_identical(cur$key(), "banana")
  expect_identical(cur$value(), "BANANA")

  expect_false(cur$move_to("cat"))
  expect_null(cur$key())
  expect_null(cur$value())
})

test_that("seek", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)
  objects <- c("apple", "banana", "carrot", "dog")
  for (i in objects) {
    txn$put(i, toupper(i))
  }

  cur <- txn$cursor()
  expect_true(cur$seek("c"))
  expect_identical(cur$key(), "carrot")
  expect_identical(cur$value(), "CARROT")

  expect_false(cur$seek("e"))
  expect_null(cur$key())
  expect_null(cur$value())
})

test_that("del", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)

  cur <- txn$cursor()
  expect_false(cur$del())
  cur$first()
  expect_false(cur$del())

  for (i in letters) {
    txn$put(i, toupper(i))
  }

  cur <- txn$cursor()
  expect_false(cur$del())

  cur$move_to("g")
  expect_identical(cur$key(), "g")
  expect_true(cur$del())
  expect_identical(cur$key(), "h")
})

test_that("put", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)
  cur <- txn$cursor()

  expect_true(cur$put("a", "A"))
  expect_false(cur$put("a", "AA", overwrite = FALSE))
  expect_equal(cur$key(), "a")
  expect_equal(cur$value(), "A")

  expect_true(cur$put("a", "AA", overwrite = TRUE))
  expect_equal(cur$key(), "a")
  expect_equal(cur$value(), "AA")
})

test_that("replace", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)
  cur <- txn$cursor()
  bytes <- as.raw(c(6, 0, 1))
  expect_null(cur$replace("a", "A"))
  expect_equal(cur$replace("a", bytes), "A")
  expect_equal(cur$replace("a", "apple"), bytes)
})

test_that("pop", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)
  for (i in letters) {
    txn$put(i, toupper(i))
  }

  cur <- txn$cursor()
  expect_identical(cur$pop("g"), "G")
  expect_true(cur$.valid)
  expect_identical(cur$key(), "h")

  expect_null(cur$pop("g"))
  expect_false(cur$.valid)
  expect_null(cur$key())
})

test_that("get", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)
  for (i in letters) {
    txn$put(i, toupper(i))
  }
  cur <- txn$cursor()
  expect_identical(cur$get("g"), "G")
  expect_equal(cur$key(), "g")
  expect_null(cur$get("apple"))

  ## Check some options work
  expect_equal(cur$get("g", as_raw = TRUE), charToRaw("G"))

  p <- cur$get("h", as_proxy = TRUE)
  expect_is(p, "mdb_val_proxy")
  expect_identical(p$value(), "H")
})
