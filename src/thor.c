#include "thor.h"
#include "util.h"

void thor_init() {
}

void thor_cleanup() {
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
  MDB_env * env = r_mdb_get_env(r_env, false);
  if (env != NULL) {
    Rprintf("Cleaning environent\n");
    mdb_env_close(env);
    R_ClearExternalPtr(r_env);
  }
}

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
