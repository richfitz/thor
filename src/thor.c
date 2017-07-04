#include "thor.h"
#include "util.h"

void thor_init() {
}

void thor_cleanup() {
}

static void r_mdb_env_finalize(SEXP r_env);
static void r_mdb_txn_finalize(SEXP r_env);

SEXP r_mdb_env_create() {
  MDB_env *env;
  no_error(mdb_env_create(&env), "mdb_env_create");

  SEXP ret = PROTECT(R_MakeExternalPtr(env, R_NilValue, R_NilValue));
  R_RegisterCFinalizer(ret, r_mdb_env_finalize);
  setAttrib(ret, R_ClassSymbol, mkString("mdb_env"));
  UNPROTECT(1);
  return ret;
}

SEXP r_mdb_env_open(SEXP r_env, SEXP r_path, SEXP r_flags) {
  bool new_env = r_env == R_NilValue;
  if (new_env) {
    r_env = PROTECT(r_mdb_env_create());
  }
  MDB_env * env = r_mdb_get_env(r_env, true);
  const char * path = scalar_character(r_path, "path");
  // TODO: more work here
  const int flags = scalar_int(r_flags, "flags");
  const mdb_mode_t mode = 0644;

  int rc = mdb_env_open(env, path, flags, mode);
  if (rc != MDB_SUCCESS) {
    // This has really weird behaviour if we use a non-new environment
    // frankly.  There is some issues here that I do not understand,
    // and it might be best to roll into this the functions that alter
    // the environment, setting things like mdb_env_set_maxdbs here.
    mdb_env_close(env);
    Rf_error("Error in mdb_env_open: %s", mdb_strerror(rc));
  }

  if (new_env) {
    UNPROTECT(1);
  }
  return r_env;
}

SEXP r_mdb_txn_begin(SEXP r_env, SEXP r_parent, SEXP r_flags) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  MDB_txn *parent =
    r_parent == R_NilValue ? NULL : r_mdb_get_txn(r_parent, true);
  const int flags = scalar_int(r_flags, "flags");

  MDB_txn *txn;
  no_error(mdb_txn_begin(env, parent, flags, &txn), "mdb_txn_begin");

  // The options here for getting the GC right are we can
  //
  // - add the env to the tag of the txn - this takes care of keeping
  //   the transaction alive in the case of accidental deletion of the
  //   handle, so we'll do that.
  // - add the txn to something in the environment so that when the
  //   environment is *forceably* closed we can abort all
  //   transactions - this is not done yet.
  SEXP ret = PROTECT(R_MakeExternalPtr(env, R_NilValue, r_env));
  R_RegisterCFinalizer(ret, r_mdb_txn_finalize);
  setAttrib(ret, R_ClassSymbol, mkString("mdb_txn"));
  UNPROTECT(1);

  return ret;
}

// --- wranglers ---
MDB_env * r_mdb_get_env(SEXP r_env, bool closed_error) {
  if (TYPEOF(r_env) != EXTPTRSXP) {
    Rf_error("Expected an external pointer");
  }
  MDB_env* env = (MDB_env*) R_ExternalPtrAddr(r_env);
  if (!env && closed_error) {
    Rf_error("mdb env is not open; can't connect");
  }
  return (MDB_env*) env;
}

MDB_txn * r_mdb_get_txn(SEXP r_txn, bool closed_error) {
  if (TYPEOF(r_txn) != EXTPTRSXP) {
    Rf_error("Expected an external pointer");
  }
  MDB_txn* txn = (MDB_txn*) R_ExternalPtrAddr(r_txn);
  if (!txn && closed_error) {
    Rf_error("mdb txn is not open; can't connect");
  }
  return (MDB_txn*) txn;
}

// --- finalizers ---

static void r_mdb_env_finalize(SEXP r_env) {
  MDB_env * env = r_mdb_get_env(r_env, false);
  if (env != NULL) {
    Rprintf("Cleaning environent\n");
    mdb_env_close(env);
    R_ClearExternalPtr(r_env);
  }
}

static void r_mdb_txn_finalize(SEXP r_txn) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, false);
  if (txn != NULL) {
    Rprintf("Cleaning transaction\n");
    // mdb_txn_abort(txn);
    R_ClearExternalPtr(r_txn);
  }
}
