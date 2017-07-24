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
