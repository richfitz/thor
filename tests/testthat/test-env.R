context("env")

test_that("create", {
  env <- mdb_env_create()
  on.exit(mdb_env_close(env))

  expect_is(env, "mdb_env")
  expect_equal(typeof(env), "externalptr")
  expect_null(mdb_env_close(env))
  on.exit()
  expect_error(mdb_env_close(env), "env has been freed; can't use")
})

test_that("create: gc", {
  env <- mdb_env_create()
  rm(env)
  gc()
})

test_that("create: gc - closed", {
  env <- mdb_env_create()
  mdb_env_close(env)
  rm(env)
  gc()
})

test_that("use unopened env", {
  env <- mdb_env_create()
  on.exit(mdb_env_close(env))

  closed_error_msg <- "Expected an opened mdb env, but recieved a closed one"

  expect_error(mdb_env_info(env), closed_error_msg)
  expect_error(mdb_env_stat(env), closed_error_msg)

  expect_equal(mdb_env_get_flags(env), NULL)
  expect_null(mdb_env_set_flags(env, NULL))

  expect_error(mdb_env_get_path(env), closed_error_msg)
  expect_null(mdb_env_set_mapsize(env, 10L))
  expect_error(mdb_env_set_maxreaders(env, 100L), closed_error_msg)
  expect_error(mdb_env_get_maxreaders(env), closed_error_msg)

  expect_null(mdb_env_set_maxdbs(env, 1000L))
  expect_equal(mdb_env_get_maxkeysize(env), 511L)
})

test_that("open: info", {
  env <- mdb_env_create()
  on.exit(mdb_env_close(env))

  path <- new_empty_dir()
  expect_null(mdb_env_open(env, path, NULL))

  stat <- mdb_env_stat(env)
  expect_is(stat, "integer")
  expect_equal(names(stat),
               c("psize", "depth", "branch_pages", "leaf_pages",
                 "overflow_pages", "entries"))

  info <- mdb_env_info(env)
  expect_is(info, "integer")
  expect_equal(names(info),
               c("mapsize", "last_pgno", "last_txnid", "maxreaders",
                 "numreaders"))
})

test_that("open: twice", {
  env <- mdb_env_create()
  on.exit(mdb_env_close(env))
  path1 <- new_empty_dir()
  path2 <- new_empty_dir()

  expect_null(mdb_env_open(env, path1, NULL))
  expect_error(mdb_env_open(env, path2, NULL),
               "Expected a closed mdb env, but recieved an opened one")
})
