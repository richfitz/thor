new_empty_dir <- function(...) {
  path <- tempfile(...)
  dir.create(path)
  path
}
