context("util_assert")

test_that("assert_character", {
  object <- NULL
  expect_error(assert_character(object), "'object' must be a character")

  expect_error(assert_character(1), "must be a character")
  expect_error(assert_character(pi), "must be a character")

  expect_silent(assert_character("fred"))
})

test_that("assert_nonmissing", {
  object <- NA
  expect_error(assert_nonmissing(object), "'object' must not be NA")

  expect_error(assert_nonmissing(NA_integer_), "must not be NA")
  expect_error(assert_nonmissing(NA_real_), "must not be NA")

  expect_silent(assert_nonmissing(TRUE))
})

test_that("assert_scalar", {
  object <- 1:5
  expect_error(assert_scalar(object), "'object' must be a scalar")

  expect_error(assert_scalar(NULL), "must be a scalar")

  expect_silent(assert_scalar(TRUE))
})

test_that("assert_logical", {
  object <- NULL
  expect_error(assert_logical(object), "'object' must be logical")

  expect_error(assert_logical(1), "must be logical")
  expect_error(assert_logical(pi), "must be logical")

  expect_silent(assert_logical(TRUE))
})

test_that("assert_is", {
  object <- NULL
  expect_error(assert_is(object, "data.frame"), "'object' must be a data.frame")

  expect_error(assert_is(1, "data.frame"), "must be a data.frame")
  expect_error(assert_is(pi, "data.frame"), "must be a data.frame")

  expect_silent(assert_is(mtcars, "data.frame"))
})
