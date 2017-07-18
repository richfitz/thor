context("transactions")

test_that("begin/abort", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)

  expect_is(txn, "transaction")
  expect_equal(mode(txn$.ptr), "externalptr")

  expect_identical(txn$.env, env)
  expect_identical(txn$.db, env$.db)
  expect_identical(env$.deps$get(), list(env$.db, txn))

  expect_identical(env$.write_txn, txn)
  expect_true(txn$.write)

  expect_identical(txn$.db$id(), 1L)
  expect_identical(txn$id(), 1L)

  expect_identical(txn$stat(), env$stat())

  ptr <- txn$.ptr
  txn$abort()
  rm(txn)
  gc()
  expect_true(is_null_pointer(ptr))

  expect_identical(env$.deps$get(), list(env$.db))
  expect_null(env$.write_txn)

  env$close()
})

test_that("basic use", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)
  expect_null(txn$put("foo", "bar"))
  expect_identical(txn$get("foo"), "bar")
  txn$commit()

  expect_identical(env$.deps$get(), list(env$.db))
  expect_null(env$.write_txn)

  txn <- env$begin(write = FALSE)
  expect_identical(txn$get("foo"), "bar")
  txn$abort()
})

test_that("concurent read", {
  env <- dbenv(tempfile())
  w1 <- env$begin(write = TRUE)

  expect_null(w1$put("foo", "bar"))
  expect_identical(w1$get("foo"), "bar")
  w1$commit()

  r1 <- env$begin(write = FALSE)
  expect_identical(r1$get("foo"), "bar")

  w2 <- env$begin(write = TRUE)
  expect_identical(r1$get("foo"), "bar")

  expect_null(w2$put("foo", "xyx"))

  expect_identical(w2$get("foo"), "xyx")
  expect_identical(r1$get("foo"), "bar")

  r2 <- env$begin(write = FALSE)
  expect_identical(r2$get("foo"), "bar")

  w2$commit()
  expect_identical(r1$get("foo"), "bar")
  expect_identical(r2$get("foo"), "bar")

  r3 <- env$begin(write = FALSE)
  expect_identical(r3$get("foo"), "xyx")

  env$close()
})

test_that("get: missing", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)

  expect_null(txn$get("foo", FALSE))
  expect_error(txn$get("foo", TRUE),
               "Key 'foo' not found in database")

  expect_null(txn$put("foo", "bar"))
  expect_identical(txn$get("foo", FALSE), "bar")
  expect_identical(txn$get("foo", TRUE), "bar")

  env$close()
})

test_that("get: string raw handling", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)
  txn$put("foo", "bar")

  expect_identical(txn$get("foo", as_raw = NULL), "bar")
  expect_identical(txn$get("foo", as_raw = FALSE), "bar")
  expect_identical(txn$get("foo", as_raw = TRUE), charToRaw("bar"))
})

test_that("get: raw raw handling", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)

  bytes <- as.raw(c(1, 51, 0, 242))

  txn$put("foo", bytes)

  expect_identical(txn$get("foo", as_raw = NULL), bytes)
  expect_identical(txn$get("foo", as_raw = TRUE), bytes)
  expect_error(txn$get("foo", as_raw = FALSE),
               "Value contains embedded nul bytes; cannot return string")
})

test_that("get: raw key", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)

  key <- as.raw(c(1, 51, 0, 242))
  value <- "hello world"
  expect_error(txn$get(key), "Key not found in database")
  expect_null(txn$get(key, FALSE))
  p <- txn$get(key, FALSE, TRUE)
  expect_null(p$value())
  expect_identical(p$size(), 0L)

  txn$put(key, value)
  expect_false(p$is_valid())
  expect_error(p$value(),
               "mdb_val_proxy is invalid: transaction has modified database")
  expect_identical(p$size(), 0L)

  p <- txn$get(key, FALSE, TRUE, FALSE)
  expect_identical(p$value(), value)
  expect_identical(p$size(), nchar(value))
  expect_identical(txn$get(key, FALSE, TRUE, NULL)$value(), value)
  expect_identical(txn$get(key, FALSE, TRUE, TRUE)$value(), charToRaw(value))
  expect_identical(txn$get(key, FALSE, TRUE, TRUE)$size(), nchar(value))

  txn$put(key, key)
  expect_false(p$is_valid())
  expect_error(p$value(),
               "mdb_val_proxy is invalid: transaction has modified database")
  expect_identical(p$size(), nchar(value))

  p <- txn$get(key, FALSE, TRUE, FALSE)
  expect_error(p$value(),
               "Value contains embedded nul bytes; cannot return string")
  p <- txn$get(key, FALSE, TRUE, NULL)
  expect_identical(p$size(), length(key))
  expect_identical(p$value(), key)
})

test_that("get: proxy", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)

  expect_null(txn$put("foo", "bar"))

  p1 <- txn$get("foo", proxy = TRUE)
  expect_is(p1, "mdb_val_proxy")
  expect_identical(p1$size(), 3L)
  expect_identical(p1$value(), "bar")
  expect_true(p1$is_valid())

  ## Let's do an update which should invalidate the proxy:
  txn$put("another", "key")
  expect_false(p1$is_valid())
  expect_error(p1$value(),
               "mdb_val_proxy is invalid: transaction has modified database")

  ## Then again:
  p2 <- txn$get("another", proxy = TRUE)
  expect_identical(p2$value(), "key")

  ## But this time we invalidate the transaction:
  txn$commit()

  expect_false(p2$is_valid())
  expect_error(p2$value(),
               "mdb_val_proxy is invalid: transaction has been closed")
})
