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
  {"Cmdb_version",                 (DL_FUNC) &r_mdb_version,               0},

  {"Cmdb_env_create",              (DL_FUNC) &r_mdb_env_create,            0},
  {"Cmdb_env_open",                (DL_FUNC) &r_mdb_env_open,              3},
  {"Cmdb_env_close",               (DL_FUNC) &r_mdb_env_close,             1},
  {"Cmdb_env_sync",                (DL_FUNC) &r_mdb_env_sync,              2},

  {"Cmdb_txn_begin",               (DL_FUNC) &r_mdb_txn_begin,             3},
  {"Cmdb_txn_id",                  (DL_FUNC) &r_mdb_txn_id,                1},
  {"Cmdb_txn_env",                 (DL_FUNC) &r_mdb_txn_env,               1},
  {"Cmdb_txn_commit",              (DL_FUNC) &r_mdb_txn_commit,            1},
  {"Cmdb_txn_abort",               (DL_FUNC) &r_mdb_txn_abort,             1},
  {"Cmdb_txn_reset",               (DL_FUNC) &r_mdb_txn_reset,             1},
  {"Cmdb_txn_renew",               (DL_FUNC) &r_mdb_txn_renew,             1},

  {"Cmdb_dbi_open",                (DL_FUNC) &r_mdb_dbi_open,              3},
  {"Cmdb_dbi_close",               (DL_FUNC) &r_mdb_dbi_close,             2},

  {"Cmdb_put",                     (DL_FUNC) &r_mdb_put,                   5},
  {"Cmdb_get",                     (DL_FUNC) &r_mdb_get,                   3},
  {"Cmdb_del",                     (DL_FUNC) &r_mdb_del,                   4},

  {"Cmdb_cursor_open",             (DL_FUNC) &r_mdb_cursor_open,           2},
  {"Cmdb_cursor_close",            (DL_FUNC) &r_mdb_cursor_close,          1},
  {"Cmdb_cursor_renew",            (DL_FUNC) &r_mdb_cursor_renew,          1},
  {"Cmdb_cursor_get",              (DL_FUNC) &r_mdb_cursor_get,            3},
  {"Cmdb_cursor_put",              (DL_FUNC) &r_mdb_cursor_put,            4},
  {"Cmdb_cursor_del",              (DL_FUNC) &r_mdb_cursor_del,            2},

  {"Cmdb_flags_env",               (DL_FUNC) &r_mdb_flags_env,             0},
  {"Cmdb_flags_dbi",               (DL_FUNC) &r_mdb_flags_dbi,             0},
  {"Cmdb_flags_write",             (DL_FUNC) &r_mdb_flags_write,           0},
  {"Cmdb_flags_copy",              (DL_FUNC) &r_mdb_flags_copy,            0},

  {"Cmdb_cursor_op",               (DL_FUNC) &r_mdb_cursor_op,             0},

  {NULL,                           NULL,                                   0}
};

void R_init_thor(DllInfo *info) {
  thor_init();
  R_registerRoutines(info, NULL, call_methods, NULL, NULL);
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 3, 0)
  /* R_useDynamicSymbols(info, FALSE); */
  /* R_forceSymbols(info, TRUE); */
#endif
}

// # nocov start
void R_unload_thor(DllInfo *info) {
  thor_cleanup();
}
// # nocov end
