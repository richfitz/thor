mdb_version <- function() {
  .Call(Cmdb_version)
}


mdb_env_create <- function() {
  .Call(Cmdb_env_create)
}


mdb_env_open <- function(env, path, mode,
                         subdir, sync, readonly,
                         metasync, writemap, lock,
                         mapasync, rdahead, meminit) {
  assert_is(mode, "octmode")
  .Call(Cmdb_env_open, env, path, mode,
        subdir, sync, readonly,
        metasync, writemap, lock,
        mapasync, rdahead, meminit)
}


mdb_env_copy <- function(env, path, compact) {
  .Call(Cmdb_env_copy, env, path, compact)
}


mdb_env_stat <- function(env) {
  .Call(Cmdb_env_stat, env)
}


mdb_env_info <- function(env) {
  ret <- .Call(Cmdb_env_info, env)
  if (ret[["mapsize"]] <= .Machine$integer.max) {
    storage.mode(ret) <- "integer"
  }
  ret
}


mdb_env_sync <- function(env, force) {
  .Call(Cmdb_env_sync, env, force)
}


mdb_env_close <- function(env) {
  .Call(Cmdb_env_close, env)
}


mdb_env_get_flags <- function(env) {
  .Call(Cmdb_env_get_flags, env)
}


mdb_env_get_path <- function(env) {
  .Call(Cmdb_env_get_path, env)
}


mdb_env_set_mapsize <- function(env, mapsize) {
  .Call(Cmdb_env_set_mapsize, env, as_integer(mapsize, TRUE))
}


mdb_env_set_maxreaders <- function(env, maxreaders) {
  .Call(Cmdb_env_set_maxreaders, env, as_integer(maxreaders))
}


mdb_env_get_maxreaders <- function(env) {
  .Call(Cmdb_env_get_maxreaders, env)
}


mdb_env_set_maxdbs <- function(env, maxdbs) {
  .Call(Cmdb_env_set_maxdbs, env, as_integer(maxdbs))
}


mdb_env_get_maxkeysize <- function(env) {
  .Call(Cmdb_env_get_maxkeysize, env)
}


## transactions:
mdb_txn_begin <- function(env, parent, readonly, sync = NULL, metasync = NULL) {
  .Call(Cmdb_txn_begin, env, parent, readonly, sync, metasync)
}


mdb_txn_id <- function(txn) {
  .Call(Cmdb_txn_id, txn)
}


mdb_txn_commit <- function(txn) {
  .Call(Cmdb_txn_commit, txn)
}


mdb_txn_abort <- function(txn, closed_error) {
  .Call(Cmdb_txn_abort, txn, closed_error)
}


mdb_txn_reset <- function(txn) {
  .Call(Cmdb_txn_reset, txn)
}


mdb_txn_renew <- function(txn) {
  .Call(Cmdb_txn_renew, txn)
}


mdb_dbi_open <- function(txn, name, reverse_key, create) {
  .Call(Cmdb_dbi_open, txn, name, reverse_key, create)
}


mdb_stat <- function(txn, dbi) {
  .Call(Cmdb_stat, txn, dbi)
}


mdb_dbi_flags <- function(txn, dbi) {
  .Call(Cmdb_dbi_flags, txn, dbi)
}


mdb_drop <- function(txn, dbi, del) {
  .Call(Cmdb_drop, txn, dbi, del)
}


mdb_get <- function(txn, dbi, key, missing_is_error, as_proxy, as_raw) {
  .Call(Cmdb_get, txn, dbi, key, missing_is_error, as_proxy, as_raw)
}


mdb_put <- function(txn, dbi, key, value, overwrite, append) {
  invisible(.Call(Cmdb_put, txn, dbi, key, value, overwrite, append))
}


mdb_del <- function(txn, dbi, key) {
  .Call(Cmdb_del, txn, dbi, key)
}


mdb_cursor_open <- function(txn, dbi) {
  .Call(Cmdb_cursor_open, txn, dbi)
}


mdb_cursor_close <- function(cursor) {
  .Call(Cmdb_cursor_close, cursor)
}


mdb_cursor_get <- function(cursor, op, key) {
  .Call(Cmdb_cursor_get, cursor, op, key)
}


mdb_cursor_put <- function(cursor, key, value, overwrite, append) {
  .Call(Cmdb_cursor_put, cursor, key, value, overwrite, append)
}


mdb_cursor_del <- function(cursor) {
  .Call(Cmdb_cursor_del, cursor)
}


mdb_cmp <- function(txn, dbi, a, b) {
  .Call(Cmdb_cmp, txn, dbi, a, b)
}


mdb_reader_list <- function(env) {
  str <- .Call(Cmdb_reader_list, env)
  if (length(str) > 1L) {
    dat <- strsplit(trimws(str), "\\s+")
    ret <- matrix(unlist(dat[-1L]), length(dat) - 1L, byrow = TRUE)
    colnames(ret) <- dat[[1L]]
  } else {
    ## This requires keeping in sync with upstream
    ret <- matrix(character(0L), 0L, 3L)
    colnames(ret) <- c("pid", "thread", "txnid")
  }
  ret
}


mdb_reader_check <- function(env) {
  .Call(Cmdb_reader_check, env)
}


## TODO: these are not mdb api but my own things; rename?
mdb_dbi_id <- function(dbi) {
  .Call(Cmdb_dbi_id, dbi)
}


mdb_proxy_copy <- function(x, as_raw) {
  .Call(Cmdb_proxy_copy, x, as_raw)
}


mdb_proxy_head <- function(x, n, as_raw) {
  .Call(Cmdb_proxy_head, x, as_integer(n), as_raw)
}


mdb_proxy_is_raw <- function(x) {
  .Call(Cmdb_proxy_is_raw, x)
}


thor_list <- function(cur, starts_with, as_raw, size) {
  .Call(Cthor_list, cur, starts_with, as_raw,
        if (!is.null(size)) as_integer(size))
}


thor_exists <- function(txn, dbi, key) {
  .Call(Cthor_exists, txn, dbi, key)
}


thor_mget <- function(txn, dbi, key, as_proxy, as_raw) {
  .Call(Cthor_mget, txn, dbi, key, as_proxy, as_raw)
}


thor_mput <- function(txn, dbi, key, value, overwrite, append) {
  invisible(.Call(Cthor_mput, txn, dbi, key, value, overwrite, append))
}


thor_mdel <- function(txn, dbi, key) {
  .Call(Cthor_mdel, txn, dbi, key)
}


pointer_addr_str <- function(x) {
  .Call(Cpointer_addr_str, x)
}
