mdb_version <- function() {
  .Call(Cmdb_version)
}

mdb_env_create <- function() {
  .Call(Cmdb_env_create)
}

mdb_env_open <- function(env, path, flags) {
  .Call(Cmdb_env_open, env, path, flags)
}

## NOTE: departure from mdb api here because not using copy2
mdb_env_copy <- function(env, path, compact = FALSE) {
  .Call(Cmdb_env_copy, env, path, compact)
}

mdb_env_stat <- function(env) {
  .Call(Cmdb_env_stat, env)
}

mdb_env_info <- function(env) {
  .Call(Cmdb_env_info, env)
}

mdb_env_sync <- function(env, force) {
  .Call(Cmdb_env_sync, env, force)
}

mdb_env_close <- function(env) {
  .Call(Cmdb_env_close, env)
}

mdb_env_get_set_flags <- function(env, flags, set = TRUE) {
  .Call(Cmdb_env_set_flags, env, flags, set)
}

mdb_env_get_get_flags <- function(env) {
  .Call(Cmdb_env_set_flags, env) # then process these!
}

mdb_env_get_get_path <- function(env) {
  .Call(Cmdb_env_set_path, env)
}

mdb_env_get_set_mapsize <- function(env, mapsize) {
  .Call(Cmdb_env_set_mapsize, env, mapsize)
}

mdb_env_get_set_maxreaders <- function(env, maxreaders) {
  .Call(Cmdb_env_set_maxreaders, env, maxreaders)
}

mdb_env_get_get_maxreaders <- function(env) {
  .Call(Cmdb_env_get_maxreaders, env)
}

mdb_env_get_set_maxdbs <- function(env, maxdbs) {
  .Call(Cmdb_env_set_maxdbs, env, maxdbs, set)
}

mdb_env_get_get_maxkeysize <- function(env) {
  .Call(Cmdb_env_get_maxkeysize, env)
}

## transactions:
mdb_txn_begin <- function(env, parent, flags) {
  .Call(Cmdb_txn_begin, env, parent, flags)
}

mdb_txn_id <- function(txn) {
  .Call(Cmdb_txn_id, txn)
}

mdb_txn_env <- function(txn) {
  .Call(Cmdb_txn_env, txn)
}

mdb_txn_commit <- function(txn) {
  .Call(Cmdb_txn_commit, txn)
}

mdb_txn_abort <- function(txn) {
  .Call(Cmdb_txn_abort, txn)
}

mdb_txn_reset <- function(txn) {
  .Call(Cmdb_txn_reset, txn)
}

mdb_txn_renew <- function(txn) {
  .Call(Cmdb_txn_renew, txn)
}

mdb_dbi_open <- function(txn, name, flags) {
  .Call(Cmdb_dbi_open, txn, name, flags)
}

mdb_dbi_close <- function(env, dbi) {
  .Call(Cmdb_dbi_close, env, dbi)
}

mdb_get <- function(txn, dbi, key) {
  .Call(Cmdb_get, txn, dbi, key)
}

mdb_put <- function(txn, dbi, key, data, flags) {
  .Call(Cmdb_put, txn, dbi, key, data, flags)
}

mdb_del <- function(txn, dbi, key, data) {
  .Call(Cmdb_del, txn, dbi, key, data)
}

mdb_cursor_open <- function(txn, dbi) {
  .Call(Cmdb_cursor_open, txn, dbi)
}

mdb_cursor_close <- function(cursor) {
  .Call(Cmdb_cursor_close, cursor)
}

mdb_cursor_renew <- function(txn, cursor) {
  .Call(Cmdb_cursor_renew, txn, cursor)
}

mdb_cursor_txn <- function(cursor) {
  .Call(Cmdb_cursor_txn, cursor)
}

mdb_cursor_dbi <- function(cursor) {
  .Call(Cmdb_cursor_dbi, cursor)
}

mdb_cursor_get <- function(cursor, key, op) {
  .Call(Cmdb_cursor_get, cursor, key, op)
}

mdb_cursor_put <- function(cursor, key, data, flags) {
  .Call(Cmdb_cursor_put, cursor, key, data, flags)
}

mdb_cursor_del <- function(cursor, flags) {
  .Call(Cmdb_cursor_del, cursor, flags)
}

mdb_cursor_count <- function(cursor) {
  .Call(Cmdb_cursor_count, cursor)
}
