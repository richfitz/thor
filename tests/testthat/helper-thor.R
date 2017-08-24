new_empty_dir <- function(...) {
  path <- tempfile(...)
  dir.create(path)
  path
}

expect_object_docs <- function(object) {
  m <- unlist(object$public_fields$.methods, use.names = FALSE)
  methods <- ls(object$public_methods)

  sys <- c("initialize", "finalize", "format")

  testthat::expect_equal(setdiff(m, methods), character(0))
  testthat::expect_equal(setdiff(methods, c(sys, m)), character(0))
}
