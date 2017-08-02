#!/usr/bin/env Rscript
lmdb_src <- dir("src/lmdb", full.names = TRUE, pattern = "\\.c$")
covr::codecov(line_exclusions = lmdb_src)
