#!/usr/bin/env Rscript
stopifnot(file.exists("DESCRIPTION"),
          read.dcf("DESCRIPTION", "Package")[[1]] == "thor",
          file.exists("src/lmdb/version"))

version <- readLines("src/lmdb/version")

url <- sprintf("https://github.com/LMDB/lmdb/archive/%s.zip", version)

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

'This the library code from lmdb - see https://github.com/LMDB/lmdb

Currently at [VERSION](https://github.com/LMDB/lmdb/tree/VERSION)

From the lmdb distribution we use the files

* `lmdb.h`
* `mdb.c`
* `midl.c`
* `midl.h`

verbatim.  The licence for these files is installed with the package - see [`inst/LICENSE.lmdb`](../../inst/LICENSE.lmdb)

To update the version, edit the [`version`](version) file in this directory, and then (from the `thor` root) run

```
./scripts/update_lmdb.R
```

To regenerate the patch, edit `src/lmdb/mdb.c` to your heart\'s content, then run

```
git diff --no-index src/lmdb/mdb.c.orig src/lmdb/mdb.c > src/lmdb/mdb.patch
```' -> template

stopifnot(file.copy(file.path(path, src), "src/lmdb", overwrite = TRUE))
stopifnot(file.copy(file.path(path, lic), "inst/LICENSE.lmdb",
                    overwrite = TRUE))
writeLines(gsub("VERSION", version, template), file.path("src/lmdb/README.md"))

stopifnot(file.copy("src/lmdb/mdb.c", "src/lmdb/mdb.c.orig", overwrite = TRUE))

message("Applying patch")
code <- system2(c("git", "apply", "src/lmdb/mdb.patch"))
if (code != 0L) {
  stop("Error applying patch")
}

message("Updated lmdb to ", version)
