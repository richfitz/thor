mdb_version <- function() {
  .Call(Cmdb_version)
}

mdb_env_create <- function() {
  .Call(Cmdb_env_create)
}
mdb_env_open <- function(env, path, flags) {
  .Call(Cmdb_env_open, env, path, flags)
}
mdb_env_close <- function(env) {
  .Call(Cmdb_env_close, env)
}

mdb_dbi_open <- function(txn, name, flags) {
  .Call(Cmdb_dbi_open, txn, name, flags)
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

mdb_put <- function(txn, dbi, key, data, flags) {
  .Call(Cmdb_put, txn, dbi, key, data, flags)
}

mdb_get <- function(txn, dbi, key) {
  .Call(Cmdb_get, txn, dbi, key)
}
