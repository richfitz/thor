#!/usr/bin/env Rscript
stopifnot(file.exists("DESCRIPTION"),
          read.dcf("DESCRIPTION", "Package")[[1]] == "thor",
          file.exists("src/lmdb"))

args <- commandArgs(TRUE)
if (length(args) != 1L) {
  stop("Usage: scripts/update_lmdb.R <version>")
}
version <- args[[1L]]

url <- sprintf("https://github.com/LMDB/lmdb/archive/LMDB_%s.zip", version)

zip <- tempfile()
download.file(url, zip)

tmp <- tempfile()
dir.create(tmp)
unzip(zip, exdir = tmp)
p <- dir(tmp, full.names = TRUE)
stopifnot(length(p) == 1L)
path <- file.path(p, "libraries/liblmdb")

src <- c("lmdb.h", "mdb.c", "midl.c", "midl.h")
lic <- "LICENSE"
stopifnot(all(file.exists(file.path(path, src))))
stopifnot(file.exists(file.path(path, lic)))

'Currently at VERSION (tag: LMDB_VERSION) - see https://github.com/LMDB/lmdb and https://github.com/LMDB/lmdb/releases

From this distribution we use the files

* `lmdb.h`
* `mdb.c`
* `midl.c`
* `midl.h`

verbatim.  The licence for these files is installed with the package - see [`inst/LICENSE.lmdb`](../../inst/LICENSE.lmdb)' -> template

stopifnot(file.copy(file.path(path, src), "src/lmdb", overwrite = TRUE))
stopifnot(file.copy(file.path(path, lic), "inst/LICENSE.lmdb",
                    overwrite = TRUE))
writeLines(gsub("VERSION", version, template), file.path("src/lmdb/README.md"))

message("Updated to ", version)
