#!/usr/bin/env Rscript
cov <- covr::package_coverage(
  line_exclusions = dir("src/lmdb", full.names = TRUE, pattern = "\\.c$"))
print(cov)
covr::report(cov, "coverage.html", browse = TRUE)
