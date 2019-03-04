context("env")

test_that("methods", {
  expect_object_docs(R6_mdb_env)
})

test_that("create & close", {
  path <- tempfile()
  env <- mdb_env(path)

  expect_true(file.exists(path))
  expect_true(file.info(path)$isdir)

  expect_is(env, "mdb_env")
  expect_is(env, "R6")
  expect_equal(mode(env$.ptr), "externalptr")

  expect_is(env$.db, "mdb_dbi")
  expect_equal(env$.deps$get(), list(env$.db))

  expect_equal(ls(env$.dbs, all.names = TRUE), character(0))
  expect_null(env$.write_txn)

  env$close()

  ## This is the expected state after closing:
  expect_null(env$.ptr)
  expect_null(env$.db)
  expect_null(env$.deps)
  expect_null(env$.dbs)
  expect_null(env$.write_txn)

  ## This is OK
  env$close()

  ## But this will cause an error
  expect_error(env$open_database(),
               "env has been cleaned up; can't use")
})

test_that("information", {
  p <- tempfile()
  env <- mdb_env(p)
  expect_true(file.exists(p))
  expect_true(file.info(p)$isdir)

  stat <- env$stat()
  expect_is(stat, "integer")
  expect_equal(names(stat),
               c("psize", "depth", "branch_pages", "leaf_pages",
                 "overflow_pages", "entries"))

  info <- env$info()
  expect_is(info, "integer")
  expect_equal(names(info),
               c("mapsize", "last_pgno", "last_txnid", "maxreaders",
                 "numreaders"))

  expect_identical(env$maxkeysize(), 511L)
  expect_identical(env$maxreaders(), 126L)

  expect_identical(env$path(), p)

  ## This test will get reused when we test setting flags and it is
  ## also going to be useful when we replace all the "no" flags.
  flags <- env$flags()
  expect_is(flags, "logical")
  expect_true(all(names(flags) %in% names(formals(mdb_env))))
  expect_equal(as.list(formals(mdb_env)[names(flags)]),
               as.list(flags))
})

test_that("no create", {
  p <- tempfile()
  expect_error(mdb_env(p, create = FALSE))
  expect_false(file.exists(p))

  ## This surprises me a bit:
  dir.create(p)
  env <- mdb_env(p, create = FALSE)
  expect_is(env, "mdb_env")
})

test_that("list readers", {
  env <- mdb_env(tempfile())
  cols <- c("pid", "thread", "txnid")
  expect_equal(env$reader_list(),
               matrix("", 0, 3, dimnames = list(NULL, cols)))
  t1 <- env$begin()
  t2 <- env$begin()
  m <- env$reader_list()
  expect_is(m, "matrix")
  expect_equal(colnames(m), cols)
  expect_equal(nrow(m), 2L)
  expect_equal(m[, "txnid"], as.character(c(t1$id(), t2$id())))
  expect_equal(m[, "pid"], rep(as.character(Sys.getpid()), 2))
  expect_match(m[, "thread"], "^[[:xdigit:]]+$")
})

test_that("subdir = FALSE", {
  base <- new_empty_dir()
  path <- tempfile(tmpdir = new_empty_dir())
  env <- mdb_env(path, subdir = FALSE)

  expect_true(file.exists(path))
  expect_false(file.info(path)$isdir)
  expect_true(file.exists(paste0(path, "-lock")))

  expect_false(env$flags()[["subdir"]])
})

test_that("some flags", {
  path <- tempfile()

  env1 <- mdb_env(path, sync = FALSE)
  expect_false(env1$flags()[["sync"]])

  env2 <- mdb_env(path, sync = TRUE)
  expect_true(env2$flags()[["sync"]])
})

test_that("copy", {
  env <- mdb_env(tempfile())
  txn <- env$begin(write = TRUE)
  txn$put("a", "A")
  txn$commit()

  path <- tempfile()
  expect_identical(env$copy(path), path)
  expect_true(file.exists(path))

  env2 <- mdb_env(path)
  txn2 <- env2$begin()
  expect_identical(txn2$get("a"), "A")
})

test_that("reader_check with no dead readers", {
  ## TODO: I could write a more ambitious version of this that spawns
  ## a new copy of R, opens the db and then kill the process.
  env <- mdb_env(tempfile())
  expect_identical(env$reader_check(), 0L)
})

test_that("open_database", {
  path <- tempfile()
  env <- mdb_env(path)
  expect_identical(env$open_database(), env$.db)
  ## This needs a much nicer error message!
  expect_error(env$open_database("foo"), "maxdbs limit")
  env$close()

  env <- mdb_env(path, maxdbs = 10)
  dbi <- env$open_database("foo")
  expect_identical(env$open_database("foo"), dbi)

  txn <- env$begin(dbi, write = TRUE)
  txn$put("a", "A")
  txn$commit()

  txn <- env$begin()
  expect_null(txn$get("a", FALSE))
  txn$abort()

  txn <- env$begin(dbi)
  expect_equal(txn$get("a", FALSE), "A")
  txn$abort()
})

test_that("begin - one write transaction only", {
  env <- mdb_env(tempfile())
  txn <- env$begin(write = TRUE)
  ## TODO: there needs to be some way of recovering from this
  ## situation (and similarly some way of keeping a global cache of
  ## envs so that we avoid a deadlock.
  expect_error(env$begin(write = TRUE),
               "Write transaction is already active for this environment")
})

test_that("sync", {
  env <- mdb_env(tempfile())
  expect_null(env$sync())
})

test_that("maxreaders", {
  env <- mdb_env(tempfile())
  n <- env$info()[["maxreaders"]]
  env$close()
  m <- n * 2L
  env <- mdb_env(tempfile(), maxreaders = m)
  expect_identical(env$info()[["maxreaders"]], m)
})

test_that("mapsize", {
  env <- mdb_env(tempfile())
  sz <- env$info()[["mapsize"]]
  env$close()
  sz2 <- sz * 2L
  env <- mdb_env(tempfile(), mapsize = sz2)
  expect_identical(env$info()[["mapsize"]], sz2)
})

test_that("serialisation does not crash", {
  env <- mdb_env(tempfile())
  expect_false(is_null_pointer(env$.ptr))
  env2 <- unserialize(serialize(env, NULL))
  expect_true(is_null_pointer(env2$.ptr))
  expect_error(env2$info(), "env has been freed; can't use")
})

## These tests exist to ensure that if something happens and the R6
## object does not completely build the cleanup is safe
test_that("naked environment can be garbage collected", {
  test_not_empty()
  path <- tempfile()
  dir.create(path)
  env_ptr <- mdb_env_create()
  mdb_env_open(env_ptr, path, as.octmode("0644"),
               NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)
  rm(env_ptr)
  gc()
})

test_that("naked unintialised environment can be garbage collected", {
  test_not_empty()
  path <- tempfile()
  dir.create(path)
  env_ptr <- mdb_env_create()
  rm(env_ptr)
  gc()
})

test_that("destroy: subdir", {
  env <- mdb_env(tempfile())
  path <- env$path()
  env$destroy()
  expect_false(file.exists(path))
})

test_that("destroy: file", {
  path <- tempfile()
  dir.create(path)
  path_db <- file.path(path, "mydb")
  env <- mdb_env(path_db, subdir = FALSE)
  env$destroy()
  expect_false(file.exists(path_db))
  expect_true(file.exists(path))
  expect_equal(dir(path), character(0))
})

test_that("format", {
  env <- mdb_env(tempfile())
  str <- format(env)
  expect_false(grepl("initialze", str))
  expect_true(grepl("<mdb_env>", str, fixed = TRUE))
  expect_true(grepl("drop_database", str, fixed = TRUE))
})

## Convenience wrappers:
test_that("put, get, del (scalar)", {
  env <- mdb_env(tempfile())
  expect_null(env$get("a", FALSE))
  expect_false(env$exists("a"))
  expect_null(env$put("a", "A"))
  expect_equal(env$list(), "a")
  expect_true(env$exists("a"))
  expect_equal(env$get("a"), "A")
  expect_true(env$del("a"))
  expect_false(env$del("a"))

  expect_error(env$del("a", "B"),
               "'value' is not allowed for databases with dupsort = FALSE",
               fixed = TRUE)
})

test_that("mput, mget, mdel (vector)", {
  env <- mdb_env(tempfile())
  expect_equal(env$mget(letters), vector("list", 26))
  expect_equal(env$exists(letters), rep(FALSE, 26))
  expect_null(env$mput(letters, LETTERS))
  expect_equal(env$exists(letters), rep(TRUE, 26))
  expect_equal(env$list(), letters)
  expect_equal(env$mget(letters, as_raw = FALSE), LETTERS)
  expect_equal(env$mget(letters, as_raw = NULL), as.list(LETTERS))
  expect_equal(env$mdel(letters), rep(TRUE, 26))
  expect_equal(env$mdel(letters), rep(FALSE, 26))

  expect_error(env$mdel("a", "B"),
               "'value' is not allowed for databases with dupsort = FALSE",
               fixed = TRUE)
})

test_that("convenience functions use pool", {
  env <- mdb_env(tempfile())
  expect_equal(env$.spare_txns$length(), 0L)
  expect_null(env$get("a", FALSE))
  expect_equal(env$.spare_txns$length(), 1L)
  expect_null(env$get("a", FALSE))
  expect_equal(env$.spare_txns$length(), 1L)
})

test_that("global environment lock", {
  path <- tempfile()
  env1 <- mdb_env(path)
  env2 <- mdb_env(normalizePath(path))
  expect_identical(env1$.path, env2$.path)

  txn1 <- env1$begin(write = TRUE)
  expect_error(env2$begin(write = TRUE),
               "Write transaction is already active for this path")
  expect_true(env1$.path %in% names(write_txns))
  txn1$abort()
  expect_false(env1$.path %in% names(write_txns))

  txn2 <- env2$begin(write = TRUE)
  expect_error(env1$begin(write = TRUE),
               "Write transaction is already active for this path")
  txn2$abort()

  if (.Platform$OS.type == "unix") {
    path3 <- tempfile()
    if (file.symlink(path, path3)) {
      env3 <- mdb_env(path3)
      expect_identical(env3$.path, env1$.path)

      txn3 <- env3$begin(write = TRUE)
      expect_error(env1$begin(write = TRUE),
                   "Write transaction is already active for this path")
      txn3$abort()
      env3$close()
    }
  }
  env2$close()
  env1$destroy()
})

test_that("with_transaction", {
  env <- mdb_env(tempfile())
  env$put("a", "hello")

  expect_equal(env$with_transaction(function(txn) {
    val <- txn$get("a")
    txn$put("a", "world")
    val
  }, write = TRUE), "hello")

  expect_equal(env$get("a"), "world")

  expect_error(env$with_transaction(function(txn) {
    txn$put("a", "again")
    stop("my error")
    val
  }, write = TRUE), "my error")

  expect_equal(env$get("a"), "world")
})


test_that("readonly", {
  skip_on_os("windows")
  path <- tempfile()
  env <- mdb_env(path)
  env$put("a", "hello")
  env$close()

  files <- dir(path, full.names = TRUE)
  Sys.chmod(files, "400")
  env <- mdb_env(path, readonly = TRUE, lock = FALSE)
  expect_equal(env$list(), "a")
  expect_equal(env$get("a"), "hello")
  expect_error(env$put("a", "goodbye"))
  env$close()

  Sys.chmod(files, "664")
  unlink(path, recursive = TRUE)
})


test_that("mdb_env with non-integer hash size", {
  large <- .Machine$integer.max + 1
  env <- mdb_env(tempfile(), mapsize = large)
  expect_equal(storage.mode(env$info()), "double")
})


test_that("corner cases for hash size", {
  ## Needs to be run on 64 bit systems
  skip_on_cran()
  skip_on_windows()
  large <- .Machine$integer.max * 2
  small <- 100
  expect_error(mdb_env(tempfile(), mapsize = -large),
               "Expected a positive size for 'size'")
  expect_error(mdb_env(tempfile(), mapsize = -small),
               "Expected a positive size for 'size'")

  env <- mdb_env(tempfile())
  expect_error(.Call(Cmdb_env_set_mapsize, env$.ptr, rep(large, 2)),
               "Expected a scalar integer for 'size'")
})
