context("thor")

test_that("mdb_version", {
  expect_identical(mdb_version(), numeric_version("0.9.70"))
})
