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