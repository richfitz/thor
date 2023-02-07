#include "thor.h"
#include <errno.h>

// These are symbols that we'll use here to avoid having to use
// `install()` at every use (following R-exts).  This is relatively
// straightforward here because of the global string cache so there is
// no GC to worry about.
SEXP thor_size_name;
void thor_init(void) {
  thor_size_name = install("size");
}

static SEXP r_mdb_env_wrap(MDB_env *env, bool opened);
static SEXP r_mdb_txn_wrap(MDB_txn *txn);
static SEXP r_mdb_dbi_wrap(MDB_dbi dbi);
static SEXP r_mdb_cursor_wrap(MDB_cursor *cursor);
static void r_mdb_env_finalize(SEXP r_env);
static void r_mdb_txn_finalize(SEXP r_txn);
static void r_mdb_dbi_finalize(SEXP r_dbi);
static void r_mdb_cursor_finalize(SEXP r_cursor);
static void r_thor_val_proxy_finalize(SEXP r_proxy);

SEXP r_mdb_version(void) {
  SEXP ret = PROTECT(allocVector(VECSXP, 1));
  setAttrib(ret, R_ClassSymbol, mkString("numeric_version"));
  SET_VECTOR_ELT(ret, 0, allocVector(INTSXP, 3));
  int *d = INTEGER(VECTOR_ELT(ret, 0));

  mdb_version(d, d + 1, d + 2);

  UNPROTECT(1);
  return ret;
}

SEXP r_mdb_env_create(void) {
  MDB_env *env;
  no_error(mdb_env_create(&env), "mdb_env_create");
  return r_mdb_env_wrap(env, false);
}

SEXP r_mdb_env_open(SEXP r_env, SEXP r_path, SEXP r_mode,
                    SEXP r_subdir, SEXP r_sync, SEXP r_readonly,
                    SEXP r_metasync, SEXP r_writemap, SEXP r_lock,
                    SEXP r_mapasync, SEXP r_rdahead, SEXP r_meminit) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  const char * path = scalar_character(r_path, "path");
  const mdb_mode_t mode = scalar_size(r_mode, "mode");
  // skipping: fixedmap, notls
  const unsigned int flags = MDB_NOTLS |
    sexp_to_flag(r_subdir,   MDB_NOSUBDIR,   "subdir",     true)  |
    sexp_to_flag(r_sync,     MDB_NOSYNC,     "sync",       true)  |
    sexp_to_flag(r_readonly, MDB_RDONLY,     "readonly",   false) |
    sexp_to_flag(r_metasync, MDB_NOMETASYNC, "metasync",   true)  |
    sexp_to_flag(r_writemap, MDB_WRITEMAP,   "writemap",   false) |
    sexp_to_flag(r_lock,     MDB_NOLOCK,     "lock",       true)  |
    sexp_to_flag(r_mapasync, MDB_MAPASYNC,   "mapasync",   false) |
    sexp_to_flag(r_rdahead,  MDB_NORDAHEAD,  "rdahead",    true)  |
    sexp_to_flag(r_meminit,  MDB_NOMEMINIT,  "meminit",    true);

  int rc = mdb_env_open(env, path, flags, mode);
  if (rc != MDB_SUCCESS) {
    mdb_env_close(env);
    R_ClearExternalPtr(r_env);
    Rf_error("Error in mdb_env_open: %s", mdb_strerror(rc));
  }

  return R_NilValue;
}

SEXP r_mdb_env_get_flags(SEXP r_env) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  unsigned int flags;
  no_error(mdb_env_get_flags(env, &flags), "mdb_env_get_flags");
  SEXP ret = PROTECT(allocVector(LGLSXP, 9));
  SEXP nms = PROTECT(allocVector(STRSXP, 9));
  int* val = INTEGER(ret);
  size_t i = 0;

  // val[i] = flag_to_bool(flags, MDB_FIXEDMAP, false);
  // SET_STRING_ELT(nms, i++, mkChar("fixedmap"));
  val[i] = flag_to_bool(flags, MDB_NOSUBDIR, true);
  SET_STRING_ELT(nms, i++, mkChar("subdir"));
  val[i] = flag_to_bool(flags, MDB_RDONLY, false);
  SET_STRING_ELT(nms, i++, mkChar("readonly"));
  val[i] = flag_to_bool(flags, MDB_WRITEMAP, false);
  SET_STRING_ELT(nms, i++, mkChar("writemap"));
  val[i] = flag_to_bool(flags, MDB_NOMETASYNC, true);
  SET_STRING_ELT(nms, i++, mkChar("metasync"));
  val[i] = flag_to_bool(flags, MDB_NOSYNC, true);
  SET_STRING_ELT(nms, i++, mkChar("sync"));
  val[i] = flag_to_bool(flags, MDB_MAPASYNC, false);
  SET_STRING_ELT(nms, i++, mkChar("mapasync"));
  // val[i] = flag_to_bool(flags, MDB_NOTLS, true);
  // SET_STRING_ELT(nms, i++, mkChar("tls"));
  val[i] = flag_to_bool(flags, MDB_NOLOCK, true);
  SET_STRING_ELT(nms, i++, mkChar("lock"));
  val[i] = flag_to_bool(flags, MDB_NORDAHEAD, true);
  SET_STRING_ELT(nms, i++, mkChar("rdahead"));
  val[i] = flag_to_bool(flags, MDB_NOMEMINIT, true);
  SET_STRING_ELT(nms, i++, mkChar("meminit"));

  setAttrib(ret, R_NamesSymbol, nms);
  UNPROTECT(2);
  return ret;
}

SEXP r_mdb_env_copy(SEXP r_env, SEXP r_path, SEXP r_compact) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  const char * path = scalar_character(r_path, "path");
  bool compact = scalar_logical(r_compact, "compact");
  unsigned int flags = compact ? MDB_CP_COMPACT : 0;
  no_error(mdb_env_copy2(env, path, flags), "mdb_env_copy");
  return R_NilValue;
}

SEXP r_mdb_env_stat(SEXP r_env) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  MDB_stat stat;
  no_error(mdb_env_stat(env, &stat), "mdb_env_stat");
  return mdb_stat_to_sexp(&stat);
}

SEXP r_mdb_env_info(SEXP r_env) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  MDB_envinfo info;
  no_error(mdb_env_info(env, &info), "mdb_env_info");

  SEXP ret = PROTECT(allocVector(REALSXP, 5));
  SEXP nms = PROTECT(allocVector(STRSXP, 5));
  double *c_ret = REAL(ret);

  c_ret[0] = info.me_mapsize;
  SET_STRING_ELT(nms, 0, mkChar("mapsize"));
  c_ret[1] = info.me_last_pgno;
  SET_STRING_ELT(nms, 1, mkChar("last_pgno"));
  c_ret[2] = info.me_last_txnid;
  SET_STRING_ELT(nms, 2, mkChar("last_txnid"));
  c_ret[3] = info.me_maxreaders;
  SET_STRING_ELT(nms, 3, mkChar("maxreaders"));
  c_ret[4] = info.me_numreaders;
  SET_STRING_ELT(nms, 4, mkChar("numreaders"));

  setAttrib(ret, R_NamesSymbol, nms);
  UNPROTECT(2);
  return ret;
}

SEXP r_mdb_env_sync(SEXP r_env, SEXP r_force) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  bool force = scalar_logical(r_force, "force");
  no_error(mdb_env_sync(env, force), "mdb_env_sync");
  return R_NilValue;
}

SEXP r_mdb_env_close(SEXP r_env) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  mdb_env_close(env);
  R_ClearExternalPtr(r_env);
  return R_NilValue;
}

SEXP r_mdb_env_get_path(SEXP r_env) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  const char *path;
  no_error(mdb_env_get_path(env, &path), "mdb_env_get_path");
  return mkString(path);
}

SEXP r_mdb_env_set_mapsize(SEXP r_env, SEXP r_size) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  mdb_size_t size = scalar_mdb_size(r_size, "size");
  no_error(mdb_env_set_mapsize(env, size), "mdb_env_set_mapsize");
  return R_NilValue;
}

SEXP r_mdb_env_set_maxreaders(SEXP r_env, SEXP r_readers) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  size_t readers = scalar_size(r_readers, "readers");
  no_error(mdb_env_set_maxreaders(env, readers), "mdb_env_set_maxreaders");
  return R_NilValue;
}

SEXP r_mdb_env_get_maxreaders(SEXP r_env) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  unsigned int readers = 0;
  no_error(mdb_env_get_maxreaders(env, &readers), "mdb_env_get_maxreaders");
  return ScalarInteger(readers);
}

SEXP r_mdb_env_set_maxdbs(SEXP r_env, SEXP r_dbs) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  size_t dbs = scalar_size(r_dbs, "dbs");
  no_error(mdb_env_set_maxdbs(env, dbs), "mdb_env_set_maxdbs");
  return R_NilValue;
}

SEXP r_mdb_env_get_maxkeysize(SEXP r_env) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  return ScalarInteger(mdb_env_get_maxkeysize(env));
}

// Transactions:
SEXP r_mdb_txn_begin(SEXP r_env, SEXP r_parent,
                     SEXP r_readonly, SEXP r_sync, SEXP r_metasync) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  MDB_txn * parent =
    r_parent == R_NilValue ? NULL : r_mdb_get_txn(r_parent, true);
  const unsigned int flags =
    sexp_to_flag(r_readonly, MDB_RDONLY, "readonly", false) |
    sexp_to_flag(r_sync, MDB_NOSYNC, "sync", true) |
    sexp_to_flag(r_metasync, MDB_NOMETASYNC, "metasync", true);

  MDB_txn *txn;
  no_error(mdb_txn_begin(env, parent, flags, &txn), "mdb_txn_begin");
  SEXP r_txn = PROTECT(r_mdb_txn_wrap(txn));

  UNPROTECT(1);
  return r_txn;
}

SEXP r_mdb_txn_id(SEXP r_txn) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  return ScalarInteger(mdb_txn_id(txn));
}

SEXP r_mdb_txn_commit(SEXP r_txn) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  R_ClearExternalPtr(r_txn);
  no_error(mdb_txn_commit(txn), "mdb_txn_commit");
  return R_NilValue;
}

SEXP r_mdb_txn_abort(SEXP r_txn, SEXP r_closed_error) {
  bool closed_error = scalar_logical(r_closed_error, "closed_error");
  MDB_txn * txn = r_mdb_get_txn(r_txn, closed_error);
  mdb_txn_abort(txn);
  R_ClearExternalPtr(r_txn);
  return R_NilValue;
}

SEXP r_mdb_txn_reset(SEXP r_txn) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  mdb_txn_reset(txn);
  return R_NilValue;
}

SEXP r_mdb_txn_renew(SEXP r_txn) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  no_error(mdb_txn_renew(txn), "mdb_txn_renew");
  return R_NilValue;
}

SEXP r_mdb_dbi_open(SEXP r_txn, SEXP r_name, SEXP r_reversekey,
		    SEXP r_create) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  const char * name =
    r_name == R_NilValue ? NULL : scalar_character(r_name, "name");

  const unsigned int flags =
    sexp_to_flag(r_reversekey, MDB_REVERSEKEY, "reversekey", false) |
    sexp_to_flag(r_create, MDB_CREATE, "create", false);

  MDB_dbi dbi;
  no_error(mdb_dbi_open(txn, name, flags, &dbi), "mdb_dbi_open");

  return r_mdb_dbi_wrap(dbi);
}

SEXP r_mdb_stat(SEXP r_txn, SEXP r_dbi) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_dbi dbi = r_mdb_get_dbi(r_dbi);
  MDB_stat stat;
  mdb_stat(txn, dbi, &stat);
  return mdb_stat_to_sexp(&stat);
}

SEXP r_mdb_dbi_flags(SEXP r_txn, SEXP r_dbi) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_dbi dbi = r_mdb_get_dbi(r_dbi);
  unsigned int flags = 0;
  no_error(mdb_dbi_flags(txn, dbi, &flags), "mdb_dbi_flags");

  SEXP ret = PROTECT(allocVector(LGLSXP, 1));
  SEXP nms = PROTECT(allocVector(STRSXP, 1));
  int* val = INTEGER(ret);
  size_t i = 0;

  val[i] = flag_to_bool(flags, MDB_REVERSEKEY, false);
  SET_STRING_ELT(nms, i++, mkChar("reversekey"));

  setAttrib(ret, R_NamesSymbol, nms);
  UNPROTECT(2);
  return ret;
}

SEXP r_mdb_drop(SEXP r_txn, SEXP r_dbi, SEXP r_del) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_dbi dbi = r_mdb_get_dbi(r_dbi);
  bool del = scalar_logical(r_del, "del");
  mdb_drop(txn, dbi, del);
  return R_NilValue;
}

// --- use the database ---
SEXP r_mdb_get(SEXP r_txn, SEXP r_dbi, SEXP r_key,
               SEXP r_missing_is_error, SEXP r_as_proxy, SEXP r_as_raw) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_dbi dbi = r_mdb_get_dbi(r_dbi);
  MDB_val key, value;
  const bool
    missing_is_error = scalar_logical(r_missing_is_error, "missing_is_error"),
    as_proxy = scalar_logical(r_as_proxy, "as_proxy");
  return_as as_raw = to_return_as(r_as_raw);
  sexp_to_mdb_val(r_key, "key", &key);

  int rc = mdb_get(txn, dbi, &key, &value);
  if (rc == MDB_NOTFOUND) {
    return mdb_missing_to_sexp(missing_is_error, r_key);
  } else {
    no_error(rc, "mdb_get");
    return mdb_val_to_sexp(&value, as_proxy, as_raw);
  }
}

SEXP r_mdb_put(SEXP r_txn, SEXP r_dbi, SEXP r_key, SEXP r_value,
               SEXP r_overwrite, SEXP r_append) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_dbi dbi = r_mdb_get_dbi(r_dbi);
  MDB_val key, value;
  const unsigned int flags =
    sexp_to_flag(r_overwrite, MDB_NOOVERWRITE, "overwrite", true) |
    sexp_to_flag(r_append, MDB_APPEND, "append", false);
  sexp_to_mdb_val(r_key, "key", &key);
  sexp_to_mdb_val(r_value, "value", &value);
  no_error(mdb_put(txn, dbi, &key, &value, flags), "mdb_put");
  return R_NilValue;
}

SEXP r_mdb_del(SEXP r_txn, SEXP r_dbi, SEXP r_key) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_dbi dbi = r_mdb_get_dbi(r_dbi);
  MDB_val key, value;
  sexp_to_mdb_val(r_key, "key", &key);
  value.mv_size = 0;
  value.mv_data = "";
  int rc = mdb_del(txn, dbi, &key, &value);
  return ScalarLogical(no_error2(rc, MDB_NOTFOUND, "mdb_del"));
}

// --- cursors ---
SEXP r_mdb_cursor_open(SEXP r_txn, SEXP r_dbi) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_dbi dbi = r_mdb_get_dbi(r_dbi);
  MDB_cursor *cursor;
  no_error(mdb_cursor_open(txn, dbi, &cursor), "mdb_cursor_open");
  return r_mdb_cursor_wrap(cursor);
}

SEXP r_mdb_cursor_close(SEXP r_cursor) {
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, true);
  mdb_cursor_close(cursor);
  R_ClearExternalPtr(r_cursor);
  return R_NilValue;
}

SEXP r_mdb_cursor_get(SEXP r_cursor, SEXP r_cursor_op, SEXP r_key) {
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, true);
  MDB_val key, value;
  MDB_cursor_op cursor_op = sexp_to_cursor_op(r_cursor_op);

  if (r_key != R_NilValue) {
    // for: SET_KEY, SET_RANGE, GET_BOTH, GET_BOTH_RANGE
    sexp_to_mdb_val(r_key, "key", &key);
  }

  int rc = mdb_cursor_get(cursor, &key, &value, cursor_op);

  SEXP ret = PROTECT(allocVector(VECSXP, 2));

  if (rc == MDB_SUCCESS) {
    const bool as_proxy = true;
    const return_as as_raw = AS_ANY;
    SET_VECTOR_ELT(ret, 0, mdb_val_to_sexp(&key, as_proxy, as_raw));
    SET_VECTOR_ELT(ret, 1, mdb_val_to_sexp(&value, as_proxy, as_raw));
  } else if (!(cursor_op == MDB_GET_CURRENT && rc == EINVAL)) {
    no_error2(rc, MDB_NOTFOUND, "mdb_cursor_get");
  }
  UNPROTECT(1);
  return ret;
}

SEXP r_mdb_cursor_put(SEXP r_cursor, SEXP r_key, SEXP r_value,
                      SEXP r_overwrite, SEXP r_append) {
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, true);
  MDB_val key, value;
  sexp_to_mdb_val(r_key, "key", &key);
  sexp_to_mdb_val(r_value, "value", &value);
  const unsigned int flags =
    sexp_to_flag(r_overwrite, MDB_NOOVERWRITE, "overwrite", true) |
    sexp_to_flag(r_append, MDB_APPEND, "append", false);
  int rc = mdb_cursor_put(cursor, &key, &value, flags);
  return ScalarLogical(no_error2(rc, MDB_KEYEXIST, "mdb_cursor_put"));
}

SEXP r_mdb_cursor_del(SEXP r_cursor) {
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, true);
  const unsigned int flags = 0;
  no_error(mdb_cursor_del(cursor, flags), "mdb_cursor_del");
  return R_NilValue;
}

SEXP r_mdb_cmp(SEXP r_txn, SEXP r_dbi, SEXP r_a, SEXP r_b) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_dbi dbi = r_mdb_get_dbi(r_dbi);
  MDB_val a, b;
  sexp_to_mdb_val(r_a, "a", &a);
  sexp_to_mdb_val(r_b, "b", &b);
  return ScalarInteger(mdb_cmp(txn, dbi, &a, &b));
}

struct reader_data {
  size_t n;
  SEXP data;
};

int mdb_reader_list_callback(const char *msg, void *ctx) {
  struct reader_data * data = (struct reader_data*) ctx;
  SET_STRING_ELT(data->data, data->n, mkChar(msg));
  data->n++;
  return 0;
}

SEXP r_mdb_reader_list(SEXP r_env) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  MDB_envinfo info;
  no_error(mdb_env_info(env, &info), "mdb_env_info");
  int n = info.me_numreaders;

  SEXP ret = PROTECT(allocVector(STRSXP, n + 1));
  struct reader_data data = {0, ret};
  mdb_reader_list(env, &mdb_reader_list_callback, &data);

  UNPROTECT(1);
  return ret;
}

SEXP r_mdb_reader_check(SEXP r_env) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  int dead = 0;
  no_error(mdb_reader_check(env, &dead), "mdb_reader_check");
  return ScalarInteger(dead);
}

// --- wranglers ---
MDB_env * r_mdb_get_env(SEXP r_env, bool closed_error) {
  void * ret = r_pointer_addr(r_env, "env", closed_error);
  return (MDB_env*) ret;
}

MDB_txn * r_mdb_get_txn(SEXP r_txn, bool closed_error) {
  return (MDB_txn*) r_pointer_addr(r_txn, "txn", closed_error);
}

MDB_dbi r_mdb_get_dbi(SEXP r_dbi) {
  MDB_dbi* data = (MDB_dbi*) r_pointer_addr(r_dbi, "dbi", true);
  return *data;
}

MDB_cursor * r_mdb_get_cursor(SEXP r_cursor, bool closed_error) {
  return (MDB_cursor*) r_pointer_addr(r_cursor, "cursor", closed_error);
}

void* r_pointer_addr(SEXP r_ptr, const char * name, bool closed_error) {
  if (r_ptr == R_NilValue) {
    Rf_error("%s has been cleaned up; can't use!", name);
  }
  void * contents = R_ExternalPtrAddr(r_ptr);
  if (closed_error && contents == NULL) {
    Rf_error("%s has been freed; can't use!", name);
  }
  return contents;
}

thor_val_proxy* r_proxy_addr(SEXP r_ptr) {
  void * contents = R_ExternalPtrAddr(r_ptr);
  if (contents == NULL) {
    Rf_error("proxy has been invalidated; can't use!");
  }
  return (thor_val_proxy*) contents;
}

// --- wrappers ---
static SEXP r_mdb_env_wrap(MDB_env *env, bool opened) {
  SEXP ret = PROTECT(R_MakeExternalPtr(env, R_NilValue, R_NilValue));
  R_RegisterCFinalizer(ret, r_mdb_env_finalize);
  setAttrib(ret, R_ClassSymbol, mkString("mdb_env"));
  UNPROTECT(1);
  return ret;
}

static SEXP r_mdb_txn_wrap(MDB_txn *txn) {
  SEXP ret = PROTECT(R_MakeExternalPtr(txn, R_NilValue, R_NilValue));
  R_RegisterCFinalizer(ret, r_mdb_txn_finalize);
  setAttrib(ret, R_ClassSymbol, mkString("mdb_txn"));
  UNPROTECT(1);
  return ret;
}

static SEXP r_mdb_dbi_wrap(MDB_dbi dbi) {
  // The options here for getting the GC right are we can
  //
  // - add the txn to the tag of the dbi - this takes care of keeping
  //   the transaction alive in the case of accidental deletion of the
  //   handle, so we'll do that.
  // - add the dbi to something in the txn so that when the
  //   transaction is *forceably* closed we can abort all
  //   connections - this is not done yet.
  MDB_dbi * data = (MDB_dbi *)Calloc(1, MDB_dbi);
  *data = dbi;

  SEXP ret = PROTECT(R_MakeExternalPtr(data, R_NilValue, R_NilValue));
  R_RegisterCFinalizer(ret, r_mdb_dbi_finalize);
  setAttrib(ret, R_ClassSymbol, mkString("mdb_dbi"));
  UNPROTECT(1);
  return ret;
}

static SEXP r_mdb_cursor_wrap(MDB_cursor *cursor) {
  SEXP ret = PROTECT(R_MakeExternalPtr(cursor, R_NilValue, R_NilValue));
  R_RegisterCFinalizer(ret, r_mdb_cursor_finalize);
  setAttrib(ret, R_ClassSymbol, mkString("mdb_cursor"));
  UNPROTECT(1);
  return ret;
}

// --- finalizers ---

static void r_mdb_env_finalize(SEXP r_env) {
  MDB_env * env = r_mdb_get_env(r_env, false);
  if (env != NULL) {
    mdb_env_close(env);
    R_ClearExternalPtr(r_env);
  }
}

static void r_mdb_txn_finalize(SEXP r_txn) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, false);
  if (txn != NULL) {
    mdb_txn_abort(txn);
    R_ClearExternalPtr(r_txn);
  }
}

static void r_mdb_dbi_finalize(SEXP r_dbi) {
  // NOTE: this frees only the pointer to the memory, but not to the
  // actual database handle; that should be done with caution and
  // we're not in a great place to deal with that yet.  This is the
  // same as python interface which simply does not implement it.
  void * data = R_ExternalPtrAddr(r_dbi);
  if (data) {
    Free(data);
    R_ClearExternalPtr(r_dbi);
  }
}

static void r_mdb_cursor_finalize(SEXP r_cursor) {
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, false);
  if (cursor != NULL) {
    mdb_cursor_close(cursor);
    R_ClearExternalPtr(r_cursor);
  }
}

static void r_thor_val_proxy_finalize(SEXP r_proxy) {
  const thor_val_proxy *proxy = (thor_val_proxy*)R_ExternalPtrAddr(r_proxy);
  if (proxy != NULL) {
    Free(proxy);
    R_ClearExternalPtr(r_proxy);
  }
}

// --- other ---
void sexp_to_mdb_val(SEXP r_x, const char *name, MDB_val *x) {
  x->mv_size = sexp_get_data(r_x, (const char**) &(x->mv_data), name);
}

SEXP mdb_val_to_sexp(MDB_val *x, bool as_proxy, return_as as_raw) {
  return as_proxy ? mdb_val_to_sexp_proxy(x) : mdb_val_to_sexp_copy(x, as_raw);
}

SEXP mdb_missing_to_sexp(bool missing_is_error, SEXP r_key) {
  if (missing_is_error) {
    if (TYPEOF(r_key) == STRSXP) {
      Rf_error("Key '%s' not found in database",
               CHAR(STRING_ELT(r_key, 0)));
    } else {
      Rf_error("Key not found in database");
    }
  }
  return R_NilValue;
}

SEXP mdb_val_to_sexp_copy(MDB_val *x, return_as as_raw) {
  return raw_string_to_sexp(x->mv_data, x->mv_size, as_raw);
}

SEXP mdb_val_to_sexp_proxy(MDB_val *x) {
  thor_val_proxy *ptr = (thor_val_proxy*) Calloc(1, thor_val_proxy);
  SEXP resolved = PROTECT(allocVector(VECSXP, 2));
  SEXP ret = PROTECT(R_MakeExternalPtr(ptr, R_NilValue, resolved));
  R_RegisterCFinalizer(ret, r_thor_val_proxy_finalize);

  ptr->size = x->mv_size;
  ptr->data = x->mv_data;
  ptr->data_contains_nul = false;
  ptr->resolved[AS_STRING] = false;
  ptr->resolved[AS_RAW] = false;

  // Not sure about this one; I might need to come back here for it
  setAttrib(ret, thor_size_name, ScalarInteger(x->mv_size));

  UNPROTECT(2);
  return ret;
}

// We'll improve this eventually - allowing conversions to different
// types (at the very least raw/string) and subsetting (get these
// bytes).  But for now, just grab the whole thing:
//
// When we do fix this up it will acquire some args like:
//
// * from/to - the section to get
// * convert - something to indicate what we convert to
//
// I've done this reasonably in 'ring' - I can probably pull the
// relevant bits out into a shared package if need be.
SEXP r_mdb_proxy_copy(SEXP r_proxy, SEXP r_as_raw) {
  thor_val_proxy *proxy = r_proxy_addr(r_proxy);
  return_as as_raw =
    mdb_proxy_check_contents(proxy, to_return_as(r_as_raw), proxy->size);

  return mdb_proxy_resolve(proxy, r_proxy, as_raw);
}

SEXP r_mdb_proxy_head(SEXP r_proxy, SEXP r_n, SEXP r_as_raw) {
  thor_val_proxy *proxy = r_proxy_addr(r_proxy);
  size_t n = scalar_size(r_n, "n");
  if (n > proxy->size) {
    n = proxy->size;
  }
  return_as as_raw = to_return_as(r_as_raw);
  as_raw = mdb_proxy_check_contents(proxy, as_raw, n);
  if (n == proxy->size) {
    return mdb_proxy_resolve(proxy, r_proxy, as_raw);
  } else {
    return raw_string_to_sexp(proxy->data, n, as_raw);
  }
}

SEXP r_mdb_proxy_is_raw(SEXP r_proxy) {
  thor_val_proxy *proxy = r_proxy_addr(r_proxy);
  if (proxy->data_contains_nul) {
    return ScalarLogical(true);
  } else if (proxy->resolved[AS_STRING]) {
    return ScalarLogical(false);
  } else {
    return R_NilValue;
  }
}

return_as mdb_proxy_check_contents(thor_val_proxy* proxy, return_as as_raw,
                                   size_t n) {
  if (as_raw == AS_ANY) {
    if (proxy->resolved[AS_STRING]) {
      as_raw = AS_STRING;
    } else if (proxy->data_contains_nul) {
      as_raw = n == proxy->size ? AS_RAW :
        is_raw_string(proxy->data, n, AS_ANY);
    } else {
      return_as data_contains_nul =
        is_raw_string(proxy->data, n, AS_ANY);
      if (data_contains_nul || n == proxy->size) {
        proxy->data_contains_nul = data_contains_nul;
      }
      as_raw = proxy->data_contains_nul ? AS_RAW : AS_STRING;
    }
  }
  return as_raw;
}

SEXP mdb_proxy_resolve(thor_val_proxy *proxy, SEXP r_proxy, return_as as_raw) {
  SEXP resolved = R_ExternalPtrProtected(r_proxy);
  SEXP ret;
  if (proxy->resolved[as_raw]) {
    ret = VECTOR_ELT(resolved, as_raw);
  } else {
    ret = raw_string_to_sexp(proxy->data, proxy->size, as_raw);
    SET_VECTOR_ELT(resolved, as_raw, ret);
    proxy->resolved[as_raw] = true;
  }
  return ret;
}

SEXP mdb_stat_to_sexp(MDB_stat *stat) {
  SEXP ret = PROTECT(allocVector(INTSXP, 6));
  SEXP nms = PROTECT(allocVector(STRSXP, 6));
  int *c_ret = INTEGER(ret);

  c_ret[0] = stat->ms_psize;
  SET_STRING_ELT(nms, 0, mkChar("psize"));
  c_ret[1] = stat->ms_depth;
  SET_STRING_ELT(nms, 1, mkChar("depth"));
  c_ret[2] = stat->ms_branch_pages;
  SET_STRING_ELT(nms, 2, mkChar("branch_pages"));
  c_ret[3] = stat->ms_leaf_pages;
  SET_STRING_ELT(nms, 3, mkChar("leaf_pages"));
  c_ret[4] = stat->ms_overflow_pages;
  SET_STRING_ELT(nms, 4, mkChar("overflow_pages"));
  c_ret[5] = stat->ms_entries;
  SET_STRING_ELT(nms, 5, mkChar("entries"));

  setAttrib(ret, R_NamesSymbol, nms);
  UNPROTECT(2);
  return ret;
}

// Flags
unsigned int sexp_to_flag(SEXP r_x, unsigned int if_set, const char *name,
                          bool invert) {
  if (r_x == R_NilValue) {
    return 0;
  } else {
    bool set = scalar_logical(r_x, name);
    return set != invert ? if_set : 0;
  }
}

bool flag_to_bool(unsigned int flags, unsigned int x, bool invert) {
  unsigned int and = flags & x;
  return invert ? and == 0 : and != 0;
}

MDB_cursor_op sexp_to_cursor_op(SEXP r_cursor_op) {
  return INTEGER(r_cursor_op)[0];
}

// --- cursor_op ---
SEXP r_mdb_cursor_op(void) {
  int n = 19;
  SEXP ret = PROTECT(allocVector(INTSXP, n));
  SEXP nms = PROTECT(allocVector(STRSXP, n));
  setAttrib(ret, R_NamesSymbol, nms);
  int i = 0;

  INTEGER(ret)[i] = MDB_FIRST;
  SET_STRING_ELT(nms, i++, mkChar("FIRST"));
  INTEGER(ret)[i] = MDB_FIRST_DUP;
  SET_STRING_ELT(nms, i++, mkChar("FIRST_DUP"));
  INTEGER(ret)[i] = MDB_GET_BOTH;
  SET_STRING_ELT(nms, i++, mkChar("GET_BOTH"));
  INTEGER(ret)[i] = MDB_GET_BOTH_RANGE;
  SET_STRING_ELT(nms, i++, mkChar("GET_BOTH_RANGE"));
  INTEGER(ret)[i] = MDB_GET_CURRENT;
  SET_STRING_ELT(nms, i++, mkChar("GET_CURRENT"));
  INTEGER(ret)[i] = MDB_GET_MULTIPLE;
  SET_STRING_ELT(nms, i++, mkChar("GET_MULTIPLE"));
  INTEGER(ret)[i] = MDB_LAST;
  SET_STRING_ELT(nms, i++, mkChar("LAST"));
  INTEGER(ret)[i] = MDB_LAST_DUP;
  SET_STRING_ELT(nms, i++, mkChar("LAST_DUP"));
  INTEGER(ret)[i] = MDB_NEXT;
  SET_STRING_ELT(nms, i++, mkChar("NEXT"));
  INTEGER(ret)[i] = MDB_NEXT_DUP;
  SET_STRING_ELT(nms, i++, mkChar("NEXT_DUP"));
  INTEGER(ret)[i] = MDB_NEXT_MULTIPLE;
  SET_STRING_ELT(nms, i++, mkChar("NEXT_MULTIPLE"));
  INTEGER(ret)[i] = MDB_NEXT_NODUP;
  SET_STRING_ELT(nms, i++, mkChar("NEXT_NODUP"));
  INTEGER(ret)[i] = MDB_PREV;
  SET_STRING_ELT(nms, i++, mkChar("PREV"));
  INTEGER(ret)[i] = MDB_PREV_DUP;
  SET_STRING_ELT(nms, i++, mkChar("PREV_DUP"));
  INTEGER(ret)[i] = MDB_PREV_NODUP;
  SET_STRING_ELT(nms, i++, mkChar("PREV_NODUP"));
  INTEGER(ret)[i] = MDB_SET;
  SET_STRING_ELT(nms, i++, mkChar("SET"));
  INTEGER(ret)[i] = MDB_SET_KEY;
  SET_STRING_ELT(nms, i++, mkChar("SET_KEY"));
  INTEGER(ret)[i] = MDB_SET_RANGE;
  SET_STRING_ELT(nms, i++, mkChar("SET_RANGE"));
  INTEGER(ret)[i] = MDB_PREV_MULTIPLE;
  SET_STRING_ELT(nms, i++, mkChar("PREV_MULTIPLE"));

  UNPROTECT(2);
  return ret;
}

SEXP r_mdb_dbi_id(SEXP r_dbi) {
  MDB_dbi dbi = r_mdb_get_dbi(r_dbi);
  return ScalarInteger((int) dbi);
}

// NOTE: because we do not in general know the size of the output I am
// using a cheeky system in which I use an _attribute_ of a vector
// 'next' to indicate the place to continue reading that vector.
// Growth is linear (not geometric).  However, we can usually just
// look the size up.
//
// work is needed here if we want to support binary keys.
SEXP r_thor_list(SEXP r_cursor, SEXP r_starts_with, SEXP r_as_raw,
                 SEXP r_size) {
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, true);

  bool filter = r_starts_with != R_NilValue;
  MDB_val starts_with;
  if (filter) {
    sexp_to_mdb_val(r_starts_with, "starts_with", &starts_with);
    if (starts_with.mv_size == 0) {
      filter = false;
    }
  }

  return_as as_raw = to_return_as(r_as_raw);
  bool out_str = as_raw == AS_STRING;
  SEXPTYPE out_type = out_str ? STRSXP : VECSXP;

  size_t size;
  if (!filter || r_size == R_NilValue) {
    MDB_txn * txn = mdb_cursor_txn(cursor);
    MDB_dbi dbi = mdb_cursor_dbi(cursor);
    MDB_stat stat;
    no_error(mdb_stat(txn, dbi, &stat), "thor_list -> mdb_env_stat");
    size = stat.ms_entries;
  } else {
    size = scalar_size(r_size, "size");
  }

  MDB_val key, value;

  SEXP ret = PROTECT(allocVector(out_type, size));
  SEXP acc = ret; // 'accumulator'

  size_t i = 0, n = 0;
  int rc;
  if (true && filter) {
    key.mv_size = starts_with.mv_size;
    key.mv_data = starts_with.mv_data;
    rc = mdb_cursor_get(cursor, &key, &value, MDB_SET_RANGE);
  } else {
    rc = mdb_cursor_get(cursor, &key, &value, MDB_FIRST);
  }

  while (rc == MDB_SUCCESS) {
    if (i == size) {
      SEXP tmp = PROTECT(allocVector(out_type, size));
      setAttrib(acc, install("next"), tmp);
      UNPROTECT(1);
      acc = tmp;
      i = 0;
    }

    if (filter && !mdb_val_starts_with(&key, &starts_with)) {
      break;
    }

    if (out_str) {
      SET_STRING_ELT(acc, i++, mdb_val_to_sexp(&key, false, AS_CHAR));
    } else {
      SET_VECTOR_ELT(acc, i++, mdb_val_to_sexp(&key, false, as_raw));
    }
    n++;
    rc = mdb_cursor_get(cursor, &key, &value, MDB_NEXT);
  }
  no_error2(rc, MDB_NOTFOUND, "thor_list");

  ret = combine_vector(ret, n);
  UNPROTECT(1);
  return ret;
}

bool mdb_val_starts_with(MDB_val *x, MDB_val *prefix) {
  return x->mv_size >= prefix->mv_size &&
    memcmp(x->mv_data, prefix->mv_data, prefix->mv_size) == 0;
}

size_t sexp_to_mdb_vals(SEXP r_x, const char *name, MDB_val **x);

SEXP r_thor_exists(SEXP r_txn, SEXP r_dbi, SEXP r_key) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_dbi dbi = r_mdb_get_dbi(r_dbi);
  MDB_val *key, value;
  size_t len = sexp_to_mdb_vals(r_key, "key", &key);
  SEXP ret = PROTECT(allocVector(LGLSXP, len));
  int * ret_data = INTEGER(ret);
  for (size_t i = 0; i < len; ++i) {
    int rc = mdb_get(txn, dbi, key + i, &value);
    ret_data[i] = rc == MDB_SUCCESS;
    no_error2(rc, MDB_NOTFOUND, "thor_exists");
  }
  UNPROTECT(1);
  return ret;
}

SEXP r_thor_mget(SEXP r_txn, SEXP r_dbi, SEXP r_key,
                 SEXP r_as_proxy, SEXP r_as_raw) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_dbi dbi = r_mdb_get_dbi(r_dbi);
  const bool as_proxy = scalar_logical(r_as_proxy, "as_proxy");
  const return_as as_raw = to_return_as(r_as_raw);
  MDB_val *key, value;
  size_t len = sexp_to_mdb_vals(r_key, "key", &key);

  bool out_str = !as_proxy && as_raw == AS_STRING;

  // TODO: in theory we could return a character vector here where
  // as_raw == AS_STRING because that is totally type stable.
  SEXP ret = PROTECT(allocVector(out_str ? STRSXP : VECSXP, len));

  for (size_t i = 0; i < len; ++i) {
    int rc = mdb_get(txn, dbi, key + i, &value);
    if (no_error2(rc, MDB_NOTFOUND, "mdb_get")) {
      if (out_str) {
        SET_STRING_ELT(ret, i, mdb_val_to_sexp(&value, false, AS_CHAR));
      } else {
        SET_VECTOR_ELT(ret, i, mdb_val_to_sexp(&value, as_proxy, as_raw));
      }
    }
  }

  UNPROTECT(1);
  return ret;
}

SEXP r_thor_mput(SEXP r_txn, SEXP r_dbi, SEXP r_key, SEXP r_value,
                 SEXP r_overwrite, SEXP r_append) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_dbi dbi = r_mdb_get_dbi(r_dbi);
  MDB_val *key, *value;
  const unsigned int flags =
    sexp_to_flag(r_overwrite, MDB_NOOVERWRITE, "overwrite", true) |
    sexp_to_flag(r_append, MDB_APPEND, "append", false);

  size_t
    len_key = sexp_to_mdb_vals(r_key, "key", &key),
    len_value = sexp_to_mdb_vals(r_value, "value", &value);
  if (len_key != len_value) {
    Rf_error("Expected %d values but recieved %d", len_key, len_value);
  }

  MDB_env *env = mdb_txn_env(txn);
  MDB_txn *txn_sub;
  no_error(mdb_txn_begin(env, txn, 0, &txn_sub), "thor_mput -> txn_begin");

  // No R api calls here; this must not throw:
  for (size_t i = 0; i < len_key; ++i) {
    int rc = mdb_put(txn_sub, dbi, key + i, value + i, flags);
    if (rc != MDB_SUCCESS) {
      mdb_txn_abort(txn_sub);
      no_error(rc, "thor_mput -> mdb_put");
    }
  }
  mdb_txn_commit(txn_sub);
  // Below here is OK though

  return R_NilValue;
}

SEXP r_thor_mdel(SEXP r_txn, SEXP r_dbi, SEXP r_key) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_dbi dbi = r_mdb_get_dbi(r_dbi);
  MDB_val *key;
  size_t len_key = sexp_to_mdb_vals(r_key, "key", &key);
  MDB_val value;
  value.mv_size = 0;
  value.mv_data = "";

  MDB_env *env = mdb_txn_env(txn);
  MDB_txn *txn_sub;

  SEXP ret = PROTECT(allocVector(LGLSXP, len_key));
  int * ret_data = INTEGER(ret);

  // No R api calls here; this must not throw:
  no_error(mdb_txn_begin(env, txn, 0, &txn_sub), "thor_mdel -> txn_begin");
  for (size_t i = 0; i < len_key; ++i) {
    int rc = mdb_del(txn_sub, dbi, key + i, &value);
    if (rc == MDB_SUCCESS) {
      ret_data[i] = 1;
    } else if (rc == MDB_NOTFOUND) {
      ret_data[i] = 0;
    } else {
      mdb_txn_abort(txn_sub);
      no_error(rc, "thor_mput -> mdb_put");
    }
  }
  mdb_txn_commit(txn_sub);
  // Below here is OK though

  UNPROTECT(1);
  return ret;
}

size_t sexp_to_mdb_vals(SEXP r_x, const char *name, MDB_val **x_ptr) {
  const size_t len = TYPEOF(r_x) == RAWSXP ? 1 : (size_t)length(r_x);

  *x_ptr = (MDB_val*)R_alloc(len, sizeof(MDB_val));
  MDB_val *x = *x_ptr;

  if (TYPEOF(r_x) == RAWSXP) {
    x[0].mv_size = length(r_x);
    x[0].mv_data = RAW(r_x);
  } else if (TYPEOF(r_x) == STRSXP) {
    for (size_t i = 0; i < len; ++i) {
      SEXP el = STRING_ELT(r_x, i);
      x[i].mv_size = length(el);
      x[i].mv_data = (void *) CHAR(el);
    }
  } else if (TYPEOF(r_x) == VECSXP) {
    for (size_t i = 0; i < len; ++i) {
      sexp_to_mdb_val(VECTOR_ELT(r_x, i), name, x + i);
    }
  } else {
    Rf_error("Invalid type; expected a character or raw vector");
  }

  return len;
}
