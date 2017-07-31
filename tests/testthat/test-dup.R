context("duplicates")

test_that("Basic duplicate use", {
  env <- dbenv(tempfile(), dupsort = TRUE)
  txn <- env$begin(write = TRUE)
  expect_true(env$.db$flags(txn)[["dupsort"]])

  ## So far so simple:
  txn$put("a", "A")
  expect_identical(txn$get("a"), "A")

  txn$put("a", "B")
  expect_identical(txn$get("a"), "A")

  cur <- txn$cursor()
  cur$move_to("a")
  expect_identical(cur$key(), "a")
  expect_identical(cur$value(), "A")
  expect_identical(cur$count(), 2L)

  expect_true(cur$move_next_dup())
  expect_identical(cur$value(), "B")
  expect_false(cur$move_next_dup())
  expect_null(cur$key())
  expect_null(cur$value())

  cur$last()
  expect_true(cur$move_prev_dup())
  expect_identical(cur$key(), "a")
  expect_identical(cur$value(), "A")
  expect_false(cur$move_prev_dup())
  expect_null(cur$key())
  expect_null(cur$value())

  txn$put("b", "x")
  txn$put("b", "y")
  txn$put("b", "z")

  cur$first()
  expect_equal(cur$key(), "a")
  expect_equal(cur$value(), "A")

  cur$move_next()
  expect_equal(cur$key(), "a")
  expect_equal(cur$value(), "B")

  cur$first()
  cur$move_next_nodup()
  expect_equal(cur$key(), "b")
  expect_equal(cur$value(), "x")

  cur$move_prev_nodup()
  expect_equal(cur$key(), "a")
  expect_equal(cur$value(), "B")

  cur$move_to("b")
  cur$move_next()
  expect_identical(cur$key(), "b")
  expect_identical(cur$value(), "y")
  expect_true(cur$first_dup())
  cur$key()
  cur$value()

  expect_true(cur$last())
  expect_true(cur$last_dup())
  expect_equal(cur$key(), "b")
  expect_equal(cur$value(), "z")

  expect_true(cur$move_to_dup("a", "B"))
  expect_identical(cur$key(), "a")
  expect_identical(cur$value(), "B")

  expect_false(cur$move_to_dup("a", "C"))
  expect_null(cur$key())
  expect_null(cur$value())

  expect_true(cur$seek_dup("b", "xyz"))
  expect_identical(cur$key(), "b")
  expect_identical(cur$value(), "y")

  expect_false(cur$seek_dup("c", "a"))
  expect_null(cur$key())
  expect_null(cur$value())
})

test_that("dcmp", {
  env <- dbenv(tempfile(), dupsort = TRUE)
  txn <- env$begin(write = TRUE)
  expect_identical(txn$dcmp("a", "b"), -1L)
  expect_identical(txn$dcmp("b", "a"),  1L)
  expect_identical(txn$dcmp("a", "a"),  0L)
})

test_that("del: with value", {
  env <- dbenv(tempfile(), dupsort = TRUE)
  txn <- env$begin(write = TRUE)
  cur <- txn$cursor()

  txn$put("a", "apple")
  txn$put("a", "avocado")
  txn$put("b", "banana")
  txn$put("b", "berry")

  expect_equal(txn$get("a"), "apple")
  expect_true(cur$move_to_dup("a", "avocado"))

  expect_true(txn$del("a", "avocado"))
  expect_false(txn$del("a", "avocado"))
  expect_equal(txn$get("a"), "apple")
  expect_false(cur$move_to_dup("a", "avocado"))
})

test_that("mdel: with value", {
  env <- dbenv(tempfile(), dupsort = TRUE)
  txn <- env$begin(write = TRUE)
  cur <- txn$cursor()

  txn$put("a", "apple")
  txn$put("a", "avocado")
  txn$put("b", "banana")
  txn$put("b", "berry")

  expect_equal(txn$mdel(c("a", "b"), c("apple", "banana")), c(TRUE, TRUE))
  expect_equal(txn$mdel(c("a", "b"), c("apple", "banana")), c(FALSE, FALSE))

  expect_identical(txn$mget(c("a", "b")), list("avocado", "berry"))

  expect_error(txn$mdel(c("a", "b"), "avocado"),
               "Expected 2 values but recieved 1")
  expect_identical(txn$mdel(c("a", "b"), rep("avocado", 2)),
                   c(TRUE, FALSE))
})
