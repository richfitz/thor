#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>
#include "lmdb.h"

void thor_init();
void thor_cleanup();

// LMDB API:
SEXP r_mdb_version();

SEXP r_mdb_env_create();
SEXP r_mdb_env_open(SEXP r_env, SEXP r_path, SEXP r_flags);
SEXP r_mdb_env_copy(SEXP r_env, SEXP r_path, SEXP r_compact);
SEXP r_mdb_env_stat(SEXP r_env);
SEXP r_mdb_env_info(SEXP r_env);
SEXP r_mdb_env_sync(SEXP r_env, SEXP r_force);
SEXP r_mdb_env_close(SEXP r_env);
SEXP r_mdb_env_set_flags(SEXP r_env, SEXP r_flags, SEXP r_set);
SEXP r_mdb_env_get_flags(SEXP r_env);
SEXP r_mdb_env_get_path(SEXP r_env);
SEXP r_mdb_env_set_mapsize(SEXP r_env, SEXP r_size);
SEXP r_mdb_env_set_maxreaders(SEXP r_env, SEXP r_readers);
SEXP r_mdb_env_get_maxreaders(SEXP r_env);
SEXP r_mdb_env_set_maxdbs(SEXP r_env, SEXP r_dbs);
SEXP r_mdb_env_get_maxkeysize(SEXP r_env);

SEXP r_mdb_txn_begin(SEXP r_env, SEXP r_parent, SEXP r_flags);
SEXP r_mdb_txn_env(SEXP r_txn);
SEXP r_mdb_txn_id(SEXP r_txn);
SEXP r_mdb_txn_commit(SEXP r_txn);
SEXP r_mdb_txn_abort(SEXP r_txn);
SEXP r_mdb_txn_reset(SEXP r_txn);
SEXP r_mdb_txn_renew(SEXP r_txn);

SEXP r_mdb_dbi_open(SEXP r_txn, SEXP r_name, SEXP r_flags);
SEXP r_mdb_dbi_close(SEXP r_env, SEXP r_txn);

SEXP r_mdb_put(SEXP r_txn, SEXP r_dbi, SEXP r_key, SEXP r_data, SEXP r_flags);
SEXP r_mdb_get(SEXP r_txn, SEXP r_dbi, SEXP r_key);
SEXP r_mdb_del(SEXP r_txn, SEXP r_dbi, SEXP r_key, SEXP r_data);

SEXP r_mdb_cursor_open(SEXP r_txn, SEXP r_dbi);
SEXP r_mdb_cursor_close(SEXP r_cursor);
SEXP r_mdb_cursor_renew(SEXP r_txn, SEXP r_cursor);
SEXP r_mdb_cursor_txn(SEXP r_cursor);
SEXP r_mdb_cursor_dbi(SEXP r_cursor);
SEXP r_mdb_cursor_get(SEXP r_cursor, SEXP r_key, SEXP r_cursor_op);
SEXP r_mdb_cursor_put(SEXP r_cursor, SEXP r_key, SEXP r_data, SEXP r_flags);
SEXP r_mdb_cursor_del(SEXP r_cursor, SEXP r_flags);
SEXP r_mdb_cursor_count(SEXP r_cursor);

// Internals:
MDB_env * r_mdb_get_env(SEXP r_env, bool closed_error);
MDB_txn * r_mdb_get_txn(SEXP r_txn, bool closed_error);
MDB_dbi * r_mdb_get_dbi(SEXP r_dbi, bool closed_error);
MDB_cursor * r_mdb_get_cursor(SEXP r_cursor, bool closed_error);

void sexp_to_mdb_val(SEXP r_x, const char *name, MDB_val *x);
SEXP mdb_val_to_sexp(MDB_val *x);

// Flags

// - export to R
SEXP r_mdb_flags_env();
SEXP r_mdb_flags_dbi();
SEXP r_mdb_flags_write();
SEXP r_mdb_flags_copy();

// - interface
unsigned int sexp_to_mdb_flags(SEXP r_flags);
MDB_cursor_op sexp_to_cursor_op(SEXP r_cursor_op);

// cursor_op
SEXP r_mdb_cursor_op();
