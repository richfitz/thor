#include "thor.h"
#include "util.h"

SEXP default_env;

void thor_init() {
  default_env = r_mdb_env_create();
  R_PreserveObject(default_env);
}

void thor_cleanup() {
  R_ReleaseObject(default_env);
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
    r_env = default_env;
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
