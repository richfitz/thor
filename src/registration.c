#include "thor.h"
#include <R_ext/Rdynload.h>
#include <Rversion.h>

// for testing:
SEXP thor_test_cleanup() {
  thor_cleanup();
  thor_init();
  return R_NilValue;
}

static const R_CallMethodDef call_methods[] = {
  {"Cmdb_env_create",              (DL_FUNC) &r_mdb_env_create,            0},
  {"Cmdb_env_open",                (DL_FUNC) &r_mdb_env_open,              3},
  {NULL,                           NULL,                                   0}
};

void R_init_thor(DllInfo *info) {
  thor_init();
  R_registerRoutines(info, NULL, call_methods, NULL, NULL);
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 3, 0)
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);
#endif
}

// # nocov start
void R_unload_thor(DllInfo *info) {
  thor_cleanup();
}
// # nocov end
