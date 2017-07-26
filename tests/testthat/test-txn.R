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
               "value contains embedded nul bytes; cannot return string")
})

test_that("get: raw key", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)

  key <- as.raw(c(1, 51, 0, 242))
  value <- "hello world"
  expect_error(txn$get(key), "Key not found in database")
  expect_null(txn$get(key, FALSE))

  p <- txn$get(key, missing_is_error = FALSE, as_proxy = TRUE)
  expect_is(p, "mdb_val_proxy")
  expect_null(p$data())
  expect_identical(p$size(), 0L)

  txn$put(key, value)
  expect_false(p$is_valid())
  expect_error(p$data(),
               "mdb_val_proxy is invalid: transaction has modified database")
  expect_error(p$size(),
               "mdb_val_proxy is invalid: transaction has modified database")

  p <- txn$get(key, as_proxy = TRUE)
  expect_is(p, "mdb_val_proxy")
  expect_identical(p$data(), value)
  expect_identical(p$size(), nchar(value))

  ## These are all done twice to stamp out possible corner cases:
  expect_identical(p$data(FALSE), value)
  expect_identical(p$data(FALSE), value)
  expect_identical(p$data(NULL), value)
  expect_identical(p$data(NULL), value)
  expect_identical(p$data(TRUE), charToRaw(value))
  expect_identical(p$data(TRUE), charToRaw(value))

  ## And again, but with different values first, to deal with how this
  ## is constructed.
  p <- txn$get(key, as_proxy = TRUE)
  expect_identical(p$data(TRUE), charToRaw(value))
  expect_identical(p$data(TRUE), charToRaw(value))
  expect_identical(p$data(FALSE), value)
  expect_identical(p$data(NULL), value)

  txn$put(key, key)
  expect_false(p$is_valid())
  expect_error(p$data(),
               "mdb_val_proxy is invalid: transaction has modified database")
  expect_error(p$size(),
               "mdb_val_proxy is invalid: transaction has modified database")

  p <- txn$get(key, as_proxy = TRUE)
  expect_error(p$data(FALSE),
               "value contains embedded nul bytes; cannot return string")
  expect_identical(p$size(), length(key))
  expect_identical(p$data(), key)
  expect_identical(p$data(TRUE), key)
})

test_that("get: proxy", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)

  value <- "bar"
  expect_null(txn$put("foo", value))

  p1 <- txn$get("foo", as_proxy = TRUE)
  expect_is(p1, "mdb_val_proxy")
  expect_identical(p1$size(), 3L)
  expect_identical(p1$data(), value)
  expect_identical(p1$data(TRUE), charToRaw(value))
  expect_true(p1$is_valid())

  ## Let's do an update which should invalidate the proxy:
  txn$put("another", "key")
  expect_false(p1$is_valid())
  expect_error(p1$data(),
               "mdb_val_proxy is invalid: transaction has modified database")

  ## Then again:
  p2 <- txn$get("another", as_proxy = TRUE)
  expect_identical(p2$data(), "key")

  ## But this time we invalidate the transaction:
  txn$commit()

  expect_false(p2$is_valid())
  expect_error(p2$data(),
               "mdb_val_proxy is invalid: transaction has been closed")
})

test_that("transaction caching", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)
  for (i in letters) {
    txn$put(i, toupper(i))
  }
  txn$commit()
  expect_identical(env$.spare_txns$get(), list())

  txn <- env$begin(write = FALSE)
  txn_ptr <- txn$.ptr

  expect_equal(txn$get("g"), "G")
  txn$abort()

  expect_identical(env$.spare_txns$get(), list(txn_ptr))
  expect_null(txn$.ptr)
  expect_error(txn$get("a"), "txn has been cleaned up")

  txn2 <- env$begin(write = FALSE)
  expect_identical(env$.spare_txns$get(), list())
  expect_identical(txn2$.ptr, txn_ptr)
  expect_equal(txn2$get("g"), "G")

  txn2$abort(FALSE)
  expect_identical(env$.spare_txns$get(), list())
  expect_true(is_null_pointer(txn_ptr))
})

test_that("del", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)
  for (i in letters) {
    txn$put(i, toupper(i))
  }
  expect_true(txn$del("a"))
  expect_false(txn$del("a"))
  env$close()
})

test_that("exists", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)
  for (i in letters) {
    txn$put(i, toupper(i))
  }
  expect_true(txn$exists("a"))
  expect_false(txn$exists("A"))
  env$close()
})

test_that("replace", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)
  for (i in letters) {
    txn$put(i, toupper(i))
  }
  expect_equal(txn$replace("g", "giraffe"), "G")
  expect_equal(txn$get("g"), "giraffe")
})

test_that("pop", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)
  for (i in letters) {
    txn$put(i, toupper(i))
  }
  expect_equal(txn$pop("g"), "G")
  expect_null(txn$pop("g"))
})

test_that("cmp", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = FALSE)

  expect_identical(txn$cmp("a", "b"), -1L)
  expect_identical(txn$cmp("b", "a"),  1L)
  expect_identical(txn$cmp("a", "a"),  0L)

  expect_error(txn$dcmp("a", "b"),
               "dcmp() is not meaningful on database with dupsort = FALSE",
               fixed = TRUE)
})

test_that("drop; invalidate as we go", {
  env <- dbenv(tempfile(), maxdbs = 10)
  db2 <- env$open_database("foo")
  expect_identical(db2$id(), 2L)

  txn <- env$begin(db = db2, write = TRUE)
  for (i in letters) {
    txn$put(i, toupper(i))
  }
  txn$commit()

  txn_read <- env$begin(db = db2)
  expect_identical(txn_read$get("a"), "A")
  cur <- txn_read$cursor()
  cur$move_to("g")
  p <- cur$value(as_proxy = TRUE)

  env$drop_database(db2)

  expect_false(p$is_valid())
  expect_null(cur$.ptr)
  expect_null(txn_read$.ptr)
  expect_null(db2$.ptr)
  expect_error(cur$first(), "cursor has been cleaned up; can't use!")
  expect_error(txn_read$cursor(), "txn has been cleaned up; can't use!")
  expect_error(db2$id(), "dbi has been cleaned up; can't use")

  expect_identical(env$.deps$get(), list(env$.db))
  expect_null(txn_read$.deps)

  expect_error(env$open_database("foo", create = FALSE),
               "MDB_NOTFOUND")
})

test_that("drop but no delete", {
  env <- dbenv(tempfile(), maxdbs = 10)
  db2 <- env$open_database("foo")

  expect_identical(db2$id(), 2L)

  txn <- env$begin(db = db2, write = TRUE)
  for (i in letters) {
    txn$put(i, toupper(i))
  }
  txn$commit()

  txn_read <- env$begin(db = db2)
  expect_identical(txn_read$get("a"), "A")
  cur <- txn_read$cursor()
  cur$move_to("g")
  p <- cur$value(as_proxy = TRUE)

  env$drop_database(db2, FALSE)

  expect_false(p$is_valid())
  expect_null(cur$.ptr)
  expect_null(txn_read$.ptr)
  expect_null(db2$.ptr)
  expect_error(cur$first(), "cursor has been cleaned up; can't use!")
  expect_error(txn_read$cursor(), "txn has been cleaned up; can't use!")
  expect_error(db2$id(), "dbi has been cleaned up; can't use")

  db3 <- env$open_database("foo", create = FALSE)
  txn_read2 <- env$begin(db = db3)
  expect_null(txn_read2$get("a", FALSE))
})

test_that("drop; root database", {
  env <- dbenv(tempfile(), maxdbs = 10)
  db <- env$open_database()
  expect_error(env$drop_database(db), "Can't delete root database")
})

test_that("drop; other environment's database", {
  env1 <- dbenv(tempfile(), maxdbs = 10)
  env2 <- dbenv(tempfile(), maxdbs = 10)
  db1 <- env1$open_database("foo")
  db2 <- env2$open_database("foo")
  expect_error(env2$drop_database(db1),
               "this is not our database")
})
