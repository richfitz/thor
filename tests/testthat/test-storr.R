context("storr")

test_that("spec", {
  test_not_empty()
  storr::test_driver(function(dr = NULL, ...)
    driver_thor(dr$env %||% mdb_env(tempfile()), ...))
})

test_that("create", {
  path <- tempfile()
  env <- mdb_env(path)
  st <- storr_thor(env)
  h <- st$set("foo", mtcars)
  expect_equal(st$list_hashes(), h)
  expect_equal(st$list(), "foo")
  st$destroy()
  expect_false(file.exists(path))
})

test_that("nontrivial prefix", {
  path <- tempfile()
  env <- mdb_env(path)
  env$put("hello", "world")

  st <- storr_thor(env, "storr:")
  st$set("foo", mtcars)

  expect_equal(st$list(), "foo")
  expect_equal(length(env$list()), 4L)

  st$destroy()
  expect_true(file.exists(path))
  expect_equal(env$list(), "hello")
})
