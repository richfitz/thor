context("cursor")

test_that("methods", {
  expect_object_docs(R6_mdb_cursor)
})

test_that("create", {
  env <- mdb_env(tempfile())
  txn <- env$begin(write = TRUE)
  cur <- txn$cursor()
  expect_is(cur, "mdb_cursor")

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
  env <- mdb_env(tempfile())
  txn <- env$begin(write = TRUE)

  for (i in letters) {
    txn$put(i, toupper(i))
  }

  cur <- txn$cursor()
  expect_null(cur$key())
  expect_null(cur$key(TRUE)$data())

  expect_true(cur$first())
  expect_identical(cur$key(), "a")
  expect_identical(cur$value(), "A")

  cur$put("foo", "bar")
  expect_identical(txn$get("foo"), "bar")

  expect_identical(cur$key(), "foo")
  expect_identical(cur$value(), "bar")
  expect_true(cur$move_next())
  expect_identical(cur$key(), "g")

  env$close()
})

test_that("cursor open close", {
  env <- mdb_env(tempfile())
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
  env <- mdb_env(tempfile())
  txn <- env$begin(write = TRUE)
  for (i in letters) {
    txn$put(i, toupper(i))
  }

  cur <- txn$cursor()
  cur$last()
  expect_identical(cur$key(), "z")
  p <- cur$key(TRUE)
  expect_is(p, "mdb_val_proxy")
  expect_identical(p$data(), "z")

  ## Then we modify the data
  txn$del("g")

  expect_false(p$is_valid())
  expect_error(p$data(), "transaction has modified database")
  expect_identical(cur$key(), "z")
})

test_that("value() refreshes proxy", {
  env <- mdb_env(tempfile())
  txn <- env$begin(write = TRUE)
  cur <- txn$cursor()
  txn$put("a", "A")

  expect_null(cur$value())
  cur$first()
  expect_equal(cur$value(), "A")
  txn$put("a", "B")
  expect_equal(cur$value(), "B")
})

test_that("iteration", {
  env <- mdb_env(tempfile())
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
  env <- mdb_env(tempfile())
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
  env <- mdb_env(tempfile())
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
  env <- mdb_env(tempfile())
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
  env <- mdb_env(tempfile())
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
  env <- mdb_env(tempfile())
  txn <- env$begin(write = TRUE)
  cur <- txn$cursor()
  bytes <- as.raw(c(6, 0, 1))
  expect_null(cur$replace("a", "A"))
  expect_equal(cur$replace("a", bytes), "A")
  expect_equal(cur$replace("a", "apple"), bytes)
})

test_that("pop", {
  env <- mdb_env(tempfile())
  txn <- env$begin(write = TRUE)
  for (i in letters) {
    txn$put(i, toupper(i))
  }

  cur <- txn$cursor()
  expect_identical(cur$pop("g"), "G")
  expect_true(cur$is_valid())
  expect_identical(cur$key(), "h")

  expect_null(cur$pop("g"))
  expect_false(cur$is_valid())
  expect_null(cur$key())
})

test_that("get", {
  env <- mdb_env(tempfile())
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
  expect_identical(p$data(), "H")
})

test_that("serialisation does not crash", {
  env <- mdb_env(tempfile())
  txn <- env$begin()
  cur <- txn$cursor()
  expect_false(is_null_pointer(cur$.ptr))
  cur2 <- unserialize(serialize(cur, NULL))
  expect_true(is_null_pointer(cur2$.ptr))
  expect_error(cur2$key(), "cursor has been freed; can't use")
})

## This test exists to ensure that if something happens and the R6
## object does not completely build the cleanup is safe
test_that("naked cursor can be garbage collected", {
  env <- mdb_env(tempfile())
  txn <- env$begin()
  cur_ptr <- mdb_cursor_open(txn$.ptr, txn$.db$.ptr)
  rm(cur_ptr)
  gc()
})

test_that("format", {
  env <- mdb_env(tempfile())
  txn <- env$begin()
  cur <- txn$cursor()
  str <- format(cur)
  expect_false(grepl("initialze", str))
  expect_true(grepl("<mdb_cursor>", str, fixed = TRUE))
  expect_true(grepl("first_dup", str, fixed = TRUE))
})
