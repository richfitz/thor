context("flags")

## Basic flag contents
test_that("env flag contents", {
  expect_is(flags_env, "mdb_flags")
  expect_is(flags_env$WRITEMAP, "mdb_flag")
  expect_equal(attr(flags_env$WRITEMAP, "group_name"), "env")
  expect_is(attr(flags_env$WRITEMAP, "group_id"), "integer")
})

test_that("dbi flag contents", {
  expect_is(flags_dbi, "mdb_flags")
  expect_is(flags_dbi$CREATE, "mdb_flag")
  expect_equal(attr(flags_dbi$CREATE, "group_name"), "dbi")
  expect_is(attr(flags_dbi$CREATE, "group_id"), "integer")
})

test_that("txn flag contents", {
  expect_is(flags_txn, "mdb_flags")
  expect_is(flags_txn$RDONLY, "mdb_flag")
  expect_equal(attr(flags_txn$RDONLY, "group_name"), "txn")
  expect_is(attr(flags_txn$RDONLY, "group_id"), "integer")
})

test_that("put flag contents", {
  expect_is(flags_put, "mdb_flags")
  expect_is(flags_put$APPEND, "mdb_flag")
  expect_equal(attr(flags_put$APPEND, "group_name"), "put")
  expect_is(attr(flags_put$APPEND, "group_id"), "integer")
})

test_that("cursor_op flag contents", {
  expect_is(cursor_op, "mdb_flags")
  expect_is(cursor_op$LAST, "mdb_flag")
  expect_equal(attr(cursor_op$LAST, "group_name"), "cursor_op")
  expect_is(attr(cursor_op$LAST, "group_id"), "integer")
})

test_that("access missing flag", {
  expect_error(flags_env$FOO, "Unknown flag 'FOO'")
  expect_error(flags_txn$FOO, "Unknown flag 'FOO'")
  expect_error(flags_dbi$FOO, "Unknown flag 'FOO'")
  expect_error(flags_put$FOO, "Unknown flag 'FOO'")
  expect_error(cursor_op$FOO, "Unknown flag 'FOO'")
  expect_error(flags_env[["FOO"]], "Unknown flag 'FOO'")
  expect_error(flags_txn[["FOO"]], "Unknown flag 'FOO'")
  expect_error(flags_dbi[["FOO"]], "Unknown flag 'FOO'")
  expect_error(flags_put[["FOO"]], "Unknown flag 'FOO'")
  expect_error(cursor_op[["FOO"]], "Unknown flag 'FOO'")
})

test_that("combine flags", {
  res <- flags_env$NOLOCK | flags_env$NOTLS
  expect_is(res, "mdb_flag")
  expect_equal(attr(res, "group_name"), "env")
  expect_identical(attr(res, "group_id"), attr(flags_env$NOLOCK, "group_id"))
  expect_equal(length(res), 2)
  expect_equal(res[[1]], as.integer(flags_env$NOLOCK))
  expect_equal(res[[2]], as.integer(flags_env$NOTLS))
})

test_that("drop duplicates", {
  expect_identical(flags_env$NOLOCK | flags_env$NOLOCK, flags_env$NOLOCK)
  expect_identical(flags_env$NOLOCK | flags_env$NOTLS | flags_env$NOLOCK,
                   flags_env$NOLOCK | flags_env$NOTLS)
})

test_that("null combine", {
  expect_identical(flags_env$NOLOCK | NULL, flags_env$NOLOCK)
  expect_identical(NULL | flags_env$NOLOCK, flags_env$NOLOCK)

})

test_that("combine invalid flags", {
  expect_error(flags_env$NOLOCK | flags_put$APPEND,
               "Expected a single group type")
  expect_error(flags_env$NOLOCK | as.integer(flags_put$APPEND),
               "Can only combine mdb_flag objects")
  expect_error(as.integer(flags_env$NOLOCK) | flags_put$APPEND,
               "Can only combine mdb_flag objects")
})

test_that("cursor_ops can't be combined", {
  expect_error(cursor_op$NEXT | cursor_op$PREV,
               "Can't combine cursor_op flags")
})

test_that("unpack flags", {
  x <- flags_env$NOLOCK | flags_env$NOTLS
  y <- bitwOr(x[[1]], x[[2]])
  res <- as_mdb_flag(y, flags_env)
  expect_equal(sort(res), sort(x))
  expect_equal(attributes(res), attributes(x))
})

test_that("unpack flags", {
  expect_identical(as_mdb_flag(as.integer(flags_env$NOLOCK), flags_env),
                   flags_env$NOLOCK)
})

test_that("unpack flags", {
  expect_identical(as_mdb_flag(0L, flags_env), NULL)
})
