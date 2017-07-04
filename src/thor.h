#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>
#include "lmdb.h"

void thor_init();
void thor_cleanup();

SEXP r_mdb_env_create();
SEXP r_mdb_env_open(SEXP r_env, SEXP r_path, SEXP r_flags);
SEXP r_mdb_txn_begin(SEXP r_env, SEXP r_parent, SEXP r_flags);

MDB_env * r_mdb_get_env(SEXP r_env, bool closed_error);
MDB_txn * r_mdb_get_txn(SEXP r_txn, bool closed_error);
/*

typedef struct {
  MDB_env *env;
  int counter;
  // or even store an _array_ of open handles?
  // thor_mdb_
} thor_mdb_env;

typedef struct {
  MDB_env *env;
  MDB_dbi dbi;
  int counter;
  // or even store an _array_ of open handles?
  // thor_mdb_
} thor_mdb_dbi;

void no_error(int x, const char* str);
thor_mdb_env* thor_get_env(SEXP extPtr, int closed_error);
thor_mdb_dbi* thor_get_dbi(SEXP extPtr, int closed_error);

*/
