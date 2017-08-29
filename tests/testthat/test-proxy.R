context("proxy")

test_that("NULL proxy", {
  txn <- list(.ptr = TRUE, .mutations = 0L)
  p <- mdb_val_proxy(txn, NULL)
  expect_is(p, "mdb_val_proxy")
  expect_null(p$data())
  expect_null(p$data(TRUE))
  expect_null(p$data(FALSE))
  expect_null(p$data(NULL))
  expect_null(p$head())
  expect_identical(p$size(), 0L)

  environment(p$size)$txn$.mutations <- 1L
  expect_error(p$data(),
               "mdb_val_proxy is invalid: transaction has modified database")
  expect_error(p$size(),
               "mdb_val_proxy is invalid: transaction has modified database")

  environment(p$size)$txn$.ptr <- NULL
  expect_error(p$data(),
               "mdb_val_proxy is invalid: transaction has been closed")
  expect_error(p$size(),
               "mdb_val_proxy is invalid: transaction has been closed")
})

test_that("serialisation does not crash", {
  env <- mdb_env(tempfile())
  txn <- env$begin(write = TRUE)
  txn$put("a", "A")
  p <- txn$get("a", as_proxy = TRUE)
  expect_false(is_null_pointer(environment(p$data)$ptr))
  p2 <- unserialize(serialize(p, NULL))
  expect_true(is_null_pointer(environment(p2$data)$ptr))
  expect_error(p2$data(), "proxy has been invalidated")
})

test_that("print", {
  env <- mdb_env(tempfile())
  txn <- env$begin(write = TRUE)
  txn$put("a", "A")
  p <- txn$get("a", as_proxy = TRUE)
  str <- paste(capture.output(print(p)), collapse = "\n")
  expect_true(grepl("<mdb_val_proxy>", str, fixed = TRUE))
  expect_true(grepl("is_valid", str, fixed = TRUE))
})

test_that("head", {
  env <- mdb_env(tempfile())
  txn <- env$begin(write = TRUE)
  txn$put("a", "apple")
  p <- txn$get("a", as_proxy = TRUE)
  expect_equal(p$head(2), "ap")
  expect_null(p$is_raw())
  expect_equal(p$head(20), "apple")
  expect_equal(p$head(p$size()), "apple")
  expect_false(p$is_raw())
})

test_that("head: with raw", {
  env <- mdb_env(tempfile())
  txn <- env$begin(write = TRUE)

  bytes <- charToRaw("hello world")
  bytes[[7]] <- as.raw(0)

  txn$put("a", bytes)
  p <- txn$get("a", as_proxy = TRUE)

  expect_null(p$is_raw())

  expect_equal(p$head(), "hello ")
  expect_null(p$is_raw())

  expect_equal(p$data(), bytes)
  expect_true(p$is_raw())

  ## but still get the string here
  expect_equal(p$head(), "hello ")
  ## and bytes here
  expect_equal(p$head(7), bytes[1:7])
})
