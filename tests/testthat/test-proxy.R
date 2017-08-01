context("proxy")

test_that("NULL proxy", {
  txn <- list(.ptr = TRUE, .mutations = 0L)
  p <- mdb_val_proxy(txn, NULL)
  expect_is(p, "mdb_val_proxy")
  expect_null(p$data())
  expect_null(p$data(TRUE))
  expect_null(p$data(FALSE))
  expect_null(p$data(NULL))
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
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)
  txn$put("a", "A")
  p <- txn$get("a", as_proxy = TRUE)
  expect_false(is_null_pointer(environment(p$data)$ptr))
  p2 <- unserialize(serialize(p, NULL))
  expect_true(is_null_pointer(environment(p2$data)$ptr))
  expect_error(p2$data(), "proxy has been invalidated")
})

test_that("print", {
  env <- dbenv(tempfile())
  txn <- env$begin(write = TRUE)
  txn$put("a", "A")
  p <- txn$get("a", as_proxy = TRUE)
  str <- paste(capture.output(print(p)), collapse = "\n")
  expect_true(grepl("<mdb_val_proxy>", str, fixed = TRUE))
  expect_true(grepl("is_valid", str, fixed = TRUE))
})
