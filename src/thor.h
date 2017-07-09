#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>
#include "lmdb.h"

typedef enum thor_flag_group {
  THOR_FLAGS_ENV,
  THOR_FLAGS_DBI,
  THOR_FLAGS_TXN,
  THOR_FLAGS_WRITE,
  THOR_CURSOR_OP
} thor_flag_group;

typedef enum thor_ptr_type {
  THOR_ENV,
  THOR_DBI,
  THOR_TXN,
  THOR_CURSOR
} thor_ptr_type;

typedef enum thor_env_state {
  THOR_ENV_OPEN,
  THOR_ENV_CLOSED,
  THOR_ENV_ANY
} thor_env_state;

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
SEXP r_mdb_stat(SEXP r_txn, SEXP r_dbi);
SEXP r_mdb_dbi_flags(SEXP r_txn, SEXP r_dbi);
SEXP r_mdb_dbi_close(SEXP r_env, SEXP r_txn);
SEXP r_mdb_drop(SEXP r_txn, SEXP r_dbi, SEXP r_del);

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
SEXP r_mdb_cursor_del(SEXP r_cursor, SEXP r_nodupdata);
SEXP r_mdb_cursor_count(SEXP r_cursor);

SEXP r_mdb_cmp(SEXP r_txn, SEXP r_dbi, SEXP r_a, SEXP r_b);
SEXP r_mdb_dcmp(SEXP r_txn, SEXP r_dbi, SEXP r_a, SEXP r_b);
SEXP r_mdb_reader_list(SEXP r_env);
SEXP r_mdb_reader_check(SEXP r_env);

// Internals:
MDB_env * r_mdb_get_env(SEXP r_env, bool closed_error, thor_env_state state);
MDB_txn * r_mdb_get_txn(SEXP r_txn, bool closed_error);
MDB_dbi * r_mdb_get_dbi(SEXP r_dbi, bool closed_error);
MDB_cursor * r_mdb_get_cursor(SEXP r_cursor, bool closed_error);
void* r_pointer_addr(SEXP r_ptr, thor_ptr_type expected, const char * name,
                     bool closed_error);

void sexp_to_mdb_val(SEXP r_x, const char *name, MDB_val *x);
SEXP mdb_val_to_sexp(MDB_val *x);
SEXP mdb_stat_to_sexp(MDB_stat *stat);

// Flags

// - export to R
SEXP r_mdb_flags_env();
SEXP r_mdb_flags_dbi();
SEXP r_mdb_flags_txn();
SEXP r_mdb_flags_write();

// - interface
unsigned int sexp_to_mdb_flags(SEXP r_flags, thor_flag_group group_id);
MDB_cursor_op sexp_to_cursor_op(SEXP r_cursor_op);

// cursor_op
SEXP r_mdb_cursor_op();
