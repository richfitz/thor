context("utils")

test_that("pairlist", {
  n <- 5
  x <- as.list(seq_len(n))
  p <- .Call(Cpairlist_create, x)
  expect_equal(p, as.pairlist(rev(x)))

  for (i in seq_len(n)) {
    expect_equal(.Call(Cpairlist_drop, p, x[[i]]),
                 as.pairlist(rev(x[-i])))
  }
})
