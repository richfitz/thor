##' @useDynLib thor, .registration = TRUE
.onLoad <- function(...) {
  init_flags()
}
