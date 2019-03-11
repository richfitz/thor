#include "thor.h"
#include <R_ext/Rdynload.h>
#include <Rversion.h>

static const R_CallMethodDef call_methods[] = {
  {"Cmdb_version",                 (DL_FUNC) &r_mdb_version,               0},

  {"Cmdb_env_create",              (DL_FUNC) &r_mdb_env_create,            0},
  {"Cmdb_env_open",                (DL_FUNC) &r_mdb_env_open,             12},
  {"Cmdb_env_copy",                (DL_FUNC) &r_mdb_env_copy,              3},
  {"Cmdb_env_stat",                (DL_FUNC) &r_mdb_env_stat,              1},
  {"Cmdb_env_info",                (DL_FUNC) &r_mdb_env_info,              1},
  {"Cmdb_env_sync",                (DL_FUNC) &r_mdb_env_sync,              2},
  {"Cmdb_env_close",               (DL_FUNC) &r_mdb_env_close,             1},
  {"Cmdb_env_get_flags",           (DL_FUNC) &r_mdb_env_get_flags,         1},
  {"Cmdb_env_get_path",            (DL_FUNC) &r_mdb_env_get_path,          1},
  {"Cmdb_env_set_mapsize",         (DL_FUNC) &r_mdb_env_set_mapsize,       2},
  {"Cmdb_env_set_maxreaders",      (DL_FUNC) &r_mdb_env_set_maxreaders,    2},
  {"Cmdb_env_get_maxreaders",      (DL_FUNC) &r_mdb_env_get_maxreaders,    1},
  {"Cmdb_env_set_maxdbs",          (DL_FUNC) &r_mdb_env_set_maxdbs,        2},
  {"Cmdb_env_get_maxkeysize",      (DL_FUNC) &r_mdb_env_get_maxkeysize,    1},

  {"Cmdb_txn_begin",               (DL_FUNC) &r_mdb_txn_begin,             5},
  {"Cmdb_txn_id",                  (DL_FUNC) &r_mdb_txn_id,                1},
  {"Cmdb_txn_commit",              (DL_FUNC) &r_mdb_txn_commit,            1},
  {"Cmdb_txn_abort",               (DL_FUNC) &r_mdb_txn_abort,             2},
  {"Cmdb_txn_reset",               (DL_FUNC) &r_mdb_txn_reset,             1},
  {"Cmdb_txn_renew",               (DL_FUNC) &r_mdb_txn_renew,             1},

  {"Cmdb_dbi_open",                (DL_FUNC) &r_mdb_dbi_open,              4},
  {"Cmdb_stat",                    (DL_FUNC) &r_mdb_stat,                  2},
  {"Cmdb_dbi_flags",               (DL_FUNC) &r_mdb_dbi_flags,             2},
  {"Cmdb_drop",                    (DL_FUNC) &r_mdb_drop,                  3},

  {"Cmdb_put",                     (DL_FUNC) &r_mdb_put,                   6},
  {"Cmdb_get",                     (DL_FUNC) &r_mdb_get,                   6},
  {"Cmdb_del",                     (DL_FUNC) &r_mdb_del,                   3},

  {"Cmdb_cursor_open",             (DL_FUNC) &r_mdb_cursor_open,           2},
  {"Cmdb_cursor_close",            (DL_FUNC) &r_mdb_cursor_close,          1},
  {"Cmdb_cursor_get",              (DL_FUNC) &r_mdb_cursor_get,            3},
  {"Cmdb_cursor_put",              (DL_FUNC) &r_mdb_cursor_put,            5},
  {"Cmdb_cursor_del",              (DL_FUNC) &r_mdb_cursor_del,            1},

  {"Cmdb_cmp",                     (DL_FUNC) &r_mdb_cmp,                   4},
  {"Cmdb_reader_list",             (DL_FUNC) &r_mdb_reader_list,           1},
  {"Cmdb_reader_check",            (DL_FUNC) &r_mdb_reader_check,          1},

  {"Cmdb_cursor_op",               (DL_FUNC) &r_mdb_cursor_op,             0},

  // Extensions
  {"Cmdb_dbi_id",                  (DL_FUNC) &r_mdb_dbi_id,                1},
  {"Cthor_list",                   (DL_FUNC) &r_thor_list,                 4},
  {"Cthor_exists",                 (DL_FUNC) &r_thor_exists,               3},
  {"Cthor_mget",                   (DL_FUNC) &r_thor_mget,                 5},
  {"Cthor_mput",                   (DL_FUNC) &r_thor_mput,                 6},
  {"Cthor_mdel",                   (DL_FUNC) &r_thor_mdel,                 3},

  // Other
  {"Cmdb_proxy_copy",              (DL_FUNC) &r_mdb_proxy_copy,            2},
  {"Cmdb_proxy_head",              (DL_FUNC) &r_mdb_proxy_head,            3},
  {"Cmdb_proxy_is_raw",            (DL_FUNC) &r_mdb_proxy_is_raw,          1},

  // Testing
  {"Cis_null_pointer",             (DL_FUNC) &r_is_null_pointer,           1},
  {"Ctest_error",                  (DL_FUNC) &r_test_error,                3},
  {"Cpointer_addr_str",            (DL_FUNC) &r_pointer_addr_str,          1},

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
