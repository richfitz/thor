mdb_env_create <- function() {
  .Call(Cmdb_env_create)
}

mdb_env_open <- function(env, path, flags) {
  .Call(Cmdb_env_open, env, path, flags)
}

mdb_txn_begin <- function(env, parent, flags) {
  .Call(Cmdb_txn_begin, env, parent, flags)
}
