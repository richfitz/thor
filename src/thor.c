#include "thor.h"
#include "util.h"

MDB_env * default_env;

void thor_init() {
  int rc = mdb_env_create(&default_env);
  if (rc != MDB_SUCCESS) {
    default_env = NULL;
    Rf_error("Error initialising mdb"); // #nocov
  }
}

void thor_cleanup() {
  // This needs to have closed all the database handles too, in order,
  // so I don't think that this is terribly clever unless we keep hold
  // of everything; there is some reference counting coming in later
  // bits so we can use that I think.  We might practically be better
  // off leaking here though.  Or we create a full SEXP version which
  // will be nicer anyway.  We'd do that with R_PreserveObject
  //
  // mdb_env_close(default_env);
}

static void r_mdb_env_finalize(SEXP r_env);

SEXP r_mdb_env_create() {
  MDB_env *env;
  no_error(mdb_env_create(&env), "mdb_env_create");

  SEXP ret = PROTECT(R_MakeExternalPtr(env, R_NilValue, R_NilValue));
  R_RegisterCFinalizer(ret, r_mdb_env_finalize);
  setAttrib(ret, R_ClassSymbol, mkString("mdb_env"));
  UNPROTECT(1);
  return ret;
}

static void r_mdb_env_finalize(SEXP r_env) {
  MDB_env * env = r_mdb_get_env(r_env, false, false);
  if (env != NULL) {
    Rprintf("Cleaning environent\n");
    mdb_env_close(env);
    R_ClearExternalPtr(r_env);
  }
}

MDB_env * r_mdb_get_env(SEXP r_env, bool use_default, bool closed_error) {
  if (use_default && r_env == R_NilValue) {
    return default_env;
  }
  if (TYPEOF(r_env) != EXTPTRSXP) {
    Rf_error("Expected an external pointer");
  }
  MDB_env* env = (MDB_env*) R_ExternalPtrAddr(r_env);
  if (!env && closed_error) {
    Rf_error("mdb env is not open; can't connect");
  }
  return (MDB_env*) env;
}
