#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>
#include "lmdb.h"

void thor_init();
void thor_cleanup();

// LMDB API:
SEXP r_mdb_env_create();
SEXP r_mdb_env_open(SEXP r_env, SEXP r_path, SEXP r_flags);
SEXP r_mdb_dbi_open(SEXP r_txn, SEXP r_name, SEXP r_flags);

SEXP r_mdb_txn_begin(SEXP r_env, SEXP r_parent, SEXP r_flags);
SEXP r_mdb_txn_env(SEXP r_txn);
SEXP r_mdb_txn_id(SEXP r_txn);
SEXP r_mdb_txn_commit(SEXP r_txn);
SEXP r_mdb_txn_abort(SEXP r_txn);

SEXP r_mdb_put(SEXP r_txn, SEXP r_dbi, SEXP r_key, SEXP r_data, SEXP r_flags);
SEXP r_mdb_get(SEXP r_txn, SEXP r_dbi, SEXP r_key);

// Internals:
MDB_env * r_mdb_get_env(SEXP r_env, bool closed_error);
MDB_txn * r_mdb_get_txn(SEXP r_txn, bool closed_error);
MDB_dbi * r_mdb_get_dbi(SEXP r_dbi, bool closed_error);

void sexp_to_mdb_val(SEXP r_x, const char *name, MDB_val *x);
SEXP mdb_val_to_sexp(MDB_val *x);
