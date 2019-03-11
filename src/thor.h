#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>
#include <lmdb.h>
#include "util.h"

typedef enum thor_flag_group {
  THOR_CURSOR_OP
} thor_flag_group;

typedef struct thor_val_proxy {
  size_t size;
  const void * data;
  bool data_contains_nul;
  bool resolved[AS_ANY];
} thor_val_proxy;

void thor_init();

// LMDB API:
SEXP r_mdb_version();

SEXP r_mdb_env_create();
SEXP r_mdb_env_open(SEXP r_env, SEXP r_path, SEXP r_mode,
                    SEXP r_subdir, SEXP r_sync, SEXP r_readonly,
                    SEXP r_metasync, SEXP r_writemap, SEXP r_lock,
                    SEXP r_mapasync, SEXP r_rdahead, SEXP r_meminit);
SEXP r_mdb_env_copy(SEXP r_env, SEXP r_path, SEXP r_compact);
SEXP r_mdb_env_stat(SEXP r_env);
SEXP r_mdb_env_info(SEXP r_env);
SEXP r_mdb_env_sync(SEXP r_env, SEXP r_force);
SEXP r_mdb_env_close(SEXP r_env);
SEXP r_mdb_env_get_flags(SEXP r_env);
SEXP r_mdb_env_get_path(SEXP r_env);
SEXP r_mdb_env_set_mapsize(SEXP r_env, SEXP r_size);
SEXP r_mdb_env_set_maxreaders(SEXP r_env, SEXP r_readers);
SEXP r_mdb_env_get_maxreaders(SEXP r_env);
SEXP r_mdb_env_set_maxdbs(SEXP r_env, SEXP r_dbs);
SEXP r_mdb_env_get_maxkeysize(SEXP r_env);

SEXP r_mdb_txn_begin(SEXP r_env, SEXP r_parent,
                     SEXP r_readonly, SEXP r_sync, SEXP r_metasync);
SEXP r_mdb_txn_id(SEXP r_txn);
SEXP r_mdb_txn_commit(SEXP r_txn);
SEXP r_mdb_txn_abort(SEXP r_txn, SEXP r_closed_error);
SEXP r_mdb_txn_reset(SEXP r_txn);
SEXP r_mdb_txn_renew(SEXP r_txn);

// TODO: support integerkey, dupfixed, integerdup, reversedup
SEXP r_mdb_dbi_open(SEXP r_txn, SEXP r_name, SEXP r_reversekey, SEXP r_create);
SEXP r_mdb_stat(SEXP r_txn, SEXP r_dbi);
SEXP r_mdb_dbi_flags(SEXP r_txn, SEXP r_dbi);
SEXP r_mdb_drop(SEXP r_txn, SEXP r_dbi, SEXP r_del);

SEXP r_mdb_put(SEXP r_txn, SEXP r_dbi, SEXP r_key, SEXP r_value,
               SEXP r_overwrite, SEXP r_append);
SEXP r_mdb_get(SEXP r_txn, SEXP r_dbi, SEXP r_key,
               SEXP r_missing_is_error, SEXP r_as_proxy, SEXP r_as_raw);
SEXP r_mdb_del(SEXP r_txn, SEXP r_dbi, SEXP r_key);

SEXP r_mdb_cursor_open(SEXP r_txn, SEXP r_dbi);
SEXP r_mdb_cursor_close(SEXP r_cursor);

SEXP r_mdb_cursor_get(SEXP r_cursor, SEXP r_cursor_op, SEXP r_key);

SEXP r_mdb_cursor_put(SEXP r_cursor, SEXP r_key, SEXP r_value,
                      SEXP r_overwrite, SEXP r_append);
SEXP r_mdb_cursor_del(SEXP r_cursor);

SEXP r_mdb_cmp(SEXP r_txn, SEXP r_dbi, SEXP r_a, SEXP r_b);
SEXP r_mdb_reader_list(SEXP r_env);
SEXP r_mdb_reader_check(SEXP r_env);

// Internals:
MDB_env * r_mdb_get_env(SEXP r_env, bool closed_error);
MDB_txn * r_mdb_get_txn(SEXP r_txn, bool closed_error);
MDB_dbi r_mdb_get_dbi(SEXP r_dbi);
MDB_cursor * r_mdb_get_cursor(SEXP r_cursor, bool closed_error);

void* r_pointer_addr(SEXP r_ptr, const char * name, bool closed_error);

void sexp_to_mdb_val(SEXP r_x, const char *name, MDB_val *x);
SEXP r_mdb_proxy_copy(SEXP r_ptr, SEXP r_as_raw);
SEXP r_mdb_proxy_head(SEXP r_ptr, SEXP r_n, SEXP r_as_raw);
SEXP r_mdb_proxy_is_raw(SEXP r_proxy);

return_as mdb_proxy_check_contents(thor_val_proxy* proxy, return_as as_raw,
                                   size_t n);
SEXP mdb_proxy_resolve(thor_val_proxy *proxy, SEXP r_proxy, return_as as_raw);

SEXP mdb_val_to_sexp(MDB_val *x, bool as_proxy, return_as as_raw);
SEXP mdb_val_to_sexp_copy(MDB_val *x, return_as as_raw);
SEXP mdb_val_to_sexp_proxy(MDB_val *x);
SEXP mdb_missing_to_sexp(bool missing_is_error, SEXP r_key);
SEXP mdb_stat_to_sexp(MDB_stat *stat);

// Flags

// - interface
unsigned int sexp_to_flag(SEXP r_x, unsigned int if_set, const char *name,
                          bool invert);
MDB_cursor_op sexp_to_cursor_op(SEXP r_cursor_op);
bool flag_to_bool(unsigned int flags, unsigned int x, bool invert);

// cursor_op
SEXP r_mdb_cursor_op();

// Some serious tidying up required through here
void cleanup_txn_cursors(SEXP r_txn);
void cleanup_cursor(SEXP r_cursor, SEXP r_txn);
void cleanup_txn(SEXP r_txn);

// Extra:
SEXP r_mdb_dbi_id(SEXP r_dbi);
SEXP r_thor_list(SEXP r_cursor, SEXP r_starts_with, SEXP r_as_raw,
                 SEXP r_size);

SEXP r_thor_exists(SEXP r_txn, SEXP r_dbi, SEXP r_key);
SEXP r_thor_mget(SEXP r_txn, SEXP r_dbi, SEXP r_key,
                 SEXP r_as_proxy, SEXP r_as_raw);
SEXP r_thor_mput(SEXP r_txn, SEXP r_dbi, SEXP r_key, SEXP r_value,
                 SEXP r_overwrite, SEXP r_append);
SEXP r_thor_mdel(SEXP r_txn, SEXP r_dbi, SEXP r_key);

// Utility
bool mdb_val_starts_with(MDB_val *x, MDB_val *prefix);
