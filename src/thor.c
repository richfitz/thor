#include "thor.h"
#include <errno.h>

// These are symbols that we'll use here to avoid having to use
// `install()` at every use (following R-exts).  This is relatively
// straightforward here because of the global string cache so there is
// no GC to worry about.
SEXP thor_flag_group_id_name;
SEXP thor_txn_readonly_name;
SEXP thor_cursor_orphan_name;
SEXP thor_size_name;

void thor_init() {
  thor_flag_group_id_name = install("group_id");
  thor_txn_readonly_name = install("readonly");
  thor_cursor_orphan_name = install("orphan");
  thor_size_name = install("size");
}

void thor_cleanup() {
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

SEXP r_mdb_version() {
  SEXP ret = PROTECT(allocVector(VECSXP, 1));
  setAttrib(ret, R_ClassSymbol, mkString("numeric_version"));
  SET_VECTOR_ELT(ret, 0, allocVector(INTSXP, 3));
  int *d = INTEGER(VECTOR_ELT(ret, 0));

  mdb_version(d, d + 1, d + 2);

  UNPROTECT(1);
  return ret;
}

SEXP r_mdb_env_create() {
  MDB_env *env;
  no_error(mdb_env_create(&env), "mdb_env_create");
  return r_mdb_env_wrap(env, false);
}

SEXP r_mdb_env_open(SEXP r_env, SEXP r_path, SEXP r_mode,
                    SEXP r_nosubdir, SEXP r_nosync, SEXP r_rdonly,
                    SEXP r_nometasync, SEXP r_writemap, SEXP r_nolock,
                    SEXP r_mapasync, SEXP r_nordahead, SEXP r_nomeminit) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  const char * path = scalar_character(r_path, "path");
  const mdb_mode_t mode = scalar_size(r_mode, "mode");
  // skipping: fixedmap, notls
  const unsigned int flags = MDB_NOTLS |
    sexp_to_flag(r_nosubdir,   MDB_NOSUBDIR,   "nosubdir",   false) |
    sexp_to_flag(r_nosync,     MDB_NOSYNC,     "nosync",     false) |
    sexp_to_flag(r_rdonly,     MDB_RDONLY,     "rdonly",     false) |
    sexp_to_flag(r_nometasync, MDB_NOMETASYNC, "nometasync", false) |
    sexp_to_flag(r_writemap,   MDB_WRITEMAP,   "writemap",   false) |
    sexp_to_flag(r_nolock,     MDB_NOLOCK,     "nolock",     false) |
    sexp_to_flag(r_mapasync,   MDB_MAPASYNC,   "mapasync",   false) |
    sexp_to_flag(r_nordahead,  MDB_NORDAHEAD,  "nordahead",  false) |
    sexp_to_flag(r_nomeminit,  MDB_NOMEMINIT,  "nomeminit",  false);

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
  val[i] = flag_to_bool(flags, MDB_NOSUBDIR, false);
  SET_STRING_ELT(nms, i++, mkChar("nosubdir"));
  val[i] = flag_to_bool(flags, MDB_RDONLY, false);
  SET_STRING_ELT(nms, i++, mkChar("rdonly"));
  val[i] = flag_to_bool(flags, MDB_WRITEMAP, false);
  SET_STRING_ELT(nms, i++, mkChar("writemap"));
  val[i] = flag_to_bool(flags, MDB_NOMETASYNC, false);
  SET_STRING_ELT(nms, i++, mkChar("nometasync"));
  val[i] = flag_to_bool(flags, MDB_NOSYNC, false);
  SET_STRING_ELT(nms, i++, mkChar("nosync"));
  val[i] = flag_to_bool(flags, MDB_MAPASYNC, false);
  SET_STRING_ELT(nms, i++, mkChar("mapasync"));
  // val[i] = flag_to_bool(flags, MDB_NOTLS, false);
  // SET_STRING_ELT(nms, i++, mkChar("NOTLS"));
  val[i] = flag_to_bool(flags, MDB_NOLOCK, false);
  SET_STRING_ELT(nms, i++, mkChar("nolock"));
  val[i] = flag_to_bool(flags, MDB_NORDAHEAD, false);
  SET_STRING_ELT(nms, i++, mkChar("nordahead"));
  val[i] = flag_to_bool(flags, MDB_NOMEMINIT, false);
  SET_STRING_ELT(nms, i++, mkChar("nomeminit"));

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

  SEXP ret = PROTECT(allocVector(INTSXP, 5));
  SEXP nms = PROTECT(allocVector(STRSXP, 5));
  int *c_ret = INTEGER(ret);

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
  size_t size = scalar_size(r_size, "size");
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
                     SEXP r_rdonly, SEXP r_nosync, SEXP r_nometasync) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  MDB_txn * parent =
    r_parent == R_NilValue ? NULL : r_mdb_get_txn(r_parent, true);
  const unsigned int flags =
    sexp_to_flag(r_rdonly, MDB_RDONLY, "rdonly", false) |
    sexp_to_flag(r_nosync, MDB_NOSYNC, "nosync", false) |
    sexp_to_flag(r_nometasync, MDB_NOMETASYNC, "nometasync", false);

  MDB_txn *txn;
  no_error(mdb_txn_begin(env, parent, flags, &txn), "mdb_txn_begin");
  SEXP r_txn = PROTECT(r_mdb_txn_wrap(txn));

  setAttrib(r_txn, thor_txn_readonly_name,
            ScalarLogical((flags & MDB_RDONLY) > 0));

  UNPROTECT(1);
  return r_txn;
}

SEXP r_mdb_txn_env(SEXP r_txn) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_env * env = mdb_txn_env(txn);
  return r_mdb_env_wrap(env, true);
}

SEXP r_mdb_txn_id(SEXP r_txn) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  return ScalarInteger(mdb_txn_id(txn));
}

SEXP r_mdb_txn_commit(SEXP r_txn) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  no_error(mdb_txn_commit(txn), "mdb_txn_commit");
  R_ClearExternalPtr(r_txn);
  return R_NilValue;
}

SEXP r_mdb_txn_abort(SEXP r_txn) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  mdb_txn_abort(txn);
  R_ClearExternalPtr(r_txn);
  return R_NilValue;
}

SEXP r_mdb_txn_reset(SEXP r_txn) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  bool txn_readonly =
    (bool)INTEGER(getAttrib(r_txn, thor_txn_readonly_name))[0];
  if (!txn_readonly) {
    Rf_error("mdb_txn_reset can be used only with read-only transactions");
  }
  mdb_txn_reset(txn);
  return R_NilValue;
}

SEXP r_mdb_txn_renew(SEXP r_txn) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  bool txn_readonly =
    (bool)INTEGER(getAttrib(r_txn, thor_txn_readonly_name))[0];
  if (!txn_readonly) {
    Rf_error("mdb_txn_reset can be used only with read-only transactions");
  }
  no_error(mdb_txn_renew(txn), "mdb_txn_renew");
  return R_NilValue;
}

SEXP r_mdb_dbi_open(SEXP r_txn, SEXP r_name,
                    SEXP r_reversekey, SEXP r_dupsort, SEXP r_create) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  const char * name =
    r_name == R_NilValue ? NULL : scalar_character(r_name, "name");

  const unsigned int flags =
    sexp_to_flag(r_reversekey, MDB_REVERSEKEY, "reversekey", false) |
    sexp_to_flag(r_dupsort, MDB_DUPSORT, "dupsort", false) |
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
  return ScalarInteger(flags);
}

// "Normally unnecessary. Use with care"
SEXP r_mdb_dbi_close(SEXP r_env, SEXP r_dbi) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  MDB_dbi dbi = r_mdb_get_dbi(r_dbi);
  mdb_dbi_close(env, dbi);
  return R_NilValue;
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
  MDB_val key, data;
  const bool
    missing_is_error = scalar_logical(r_missing_is_error, "missing_is_error"),
    as_proxy = scalar_logical(r_as_proxy, "as_proxy");
  return_as as_raw = to_return_as(r_as_raw);
  sexp_to_mdb_val(r_key, "key", &key);

  int rc = mdb_get(txn, dbi, &key, &data);
  if (rc == MDB_NOTFOUND) {
    return mdb_missing_to_sexp(as_proxy, missing_is_error, R_NilValue, r_key);
  } else {
    no_error(rc, "mdb_get");
    return mdb_val_to_sexp(&data, as_proxy, as_raw);
  }
}

SEXP r_mdb_put(SEXP r_txn, SEXP r_dbi, SEXP r_key, SEXP r_data,
               SEXP r_nodupdata, SEXP r_nooverwrite, SEXP r_append) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_dbi dbi = r_mdb_get_dbi(r_dbi);
  MDB_val key, data;
  const unsigned int flags =
    sexp_to_flag(r_nodupdata, MDB_NODUPDATA, "nodupdata", false) |
    sexp_to_flag(r_nooverwrite, MDB_NOOVERWRITE, "nooverwrite", false) |
    sexp_to_flag(r_append, MDB_APPEND, "append", false);
  sexp_to_mdb_val(r_key, "key", &key);
  sexp_to_mdb_val(r_data, "data", &data);
  no_error(mdb_put(txn, dbi, &key, &data, flags), "mdb_put");
  return R_NilValue;
}

SEXP r_mdb_del(SEXP r_txn, SEXP r_dbi, SEXP r_key, SEXP r_data) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_dbi dbi = r_mdb_get_dbi(r_dbi);
  MDB_val key, data;
  sexp_to_mdb_val(r_key, "key", &key);
  if (r_data == NULL) {
    data.mv_size = 0;
    data.mv_data = "";
  } else {
    sexp_to_mdb_val(r_data, "data", &data);
  }
  no_error(mdb_del(txn, dbi, &key, &data), "mdb_del");
  return R_NilValue;
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
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, true, false);
  mdb_cursor_close(cursor);
  return R_NilValue;
}

SEXP r_mdb_cursor_renew(SEXP r_txn, SEXP r_cursor) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, true, true);
  mdb_cursor_renew(txn, cursor);
  setAttrib(r_cursor, thor_cursor_orphan_name, R_NilValue);
  return R_NilValue;
}

SEXP r_mdb_cursor_txn(SEXP r_cursor) {
  Rf_error("don't call this"); // TODO
  return R_NilValue;
}

SEXP r_mdb_cursor_dbi(SEXP r_cursor) {
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, true, false);
  MDB_dbi dbi = mdb_cursor_dbi(cursor);
  return r_mdb_dbi_wrap(dbi);
}

// TODO: Need to handle (amongst other things) missing values properly
// here.  This is an issue in particular where we sail off the end of
// the iteration.
SEXP r_mdb_cursor_get(SEXP r_cursor, SEXP r_cursor_op, SEXP r_key) {
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, true, false);
  MDB_val key, data;
  MDB_cursor_op cursor_op = sexp_to_cursor_op(r_cursor_op);

  if (r_key != R_NilValue) {
    if (cursor_op != MDB_SET_KEY) { // should not be needed when we're done
      Rf_error("key is allowed only with MDB_SET");
    }
    sexp_to_mdb_val(r_key, "key", &key);
  }

  int rc = mdb_cursor_get(cursor, &key, &data, cursor_op);
  // Possibly this would be nicer to do if we store something to
  // indicate if the cursor has ever been positioned?

  SEXP ret = PROTECT(allocVector(VECSXP, 2));

  if (rc == MDB_SUCCESS) {
    const bool as_proxy = true;
    const return_as as_raw = AS_ANY;
    SET_VECTOR_ELT(ret, 0, mdb_val_to_sexp(&key, as_proxy, as_raw));
    SET_VECTOR_ELT(ret, 1, mdb_val_to_sexp(&data, as_proxy, as_raw));
  } else {
    bool ok = rc == MDB_NOTFOUND ||
      (rc == EINVAL && cursor_op == MDB_GET_CURRENT);
    if (!ok) {
      no_error(rc, "mdb_cursor_get");
    }
  }
  UNPROTECT(1);
  return ret;
}

SEXP r_mdb_cursor_put(SEXP r_cursor, SEXP r_key, SEXP r_data,
                      SEXP r_nodupdata, SEXP r_nooverwrite, SEXP r_append) {
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, true, false);
  MDB_val key, data;
  sexp_to_mdb_val(r_key, "key", &key);
  sexp_to_mdb_val(r_data, "data", &data);
  const unsigned int flags =
    sexp_to_flag(r_nodupdata, MDB_NODUPDATA, "nodupdata", false) |
    sexp_to_flag(r_nooverwrite, MDB_NOOVERWRITE, "nooverwrite", false) |
    sexp_to_flag(r_append, MDB_APPEND, "append", false);
  no_error(mdb_cursor_put(cursor, &key, &data, flags), "mdb_cursor_put");
  return R_NilValue;
}

SEXP r_mdb_cursor_del(SEXP r_cursor, SEXP r_nodupdata) {
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, true, false);
  const bool nodupdata = scalar_logical(r_nodupdata, "nodupdata");
  const unsigned int flags = nodupdata ? MDB_NODUPDATA : 0;
  no_error(mdb_cursor_del(cursor, flags), "mdb_cursor_del");
  return R_NilValue;
}

SEXP r_mdb_cursor_count(SEXP r_cursor) {
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, true, false);
  mdb_size_t count;
  no_error(mdb_cursor_count(cursor, &count), "mdb_cursor_count");
  return ScalarInteger(count);
}

SEXP r_mdb_cmp(SEXP r_txn, SEXP r_dbi, SEXP r_a, SEXP r_b) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_dbi dbi = r_mdb_get_dbi(r_dbi);
  MDB_val a, b;
  sexp_to_mdb_val(r_a, "a", &a);
  sexp_to_mdb_val(r_b, "b", &b);
  return ScalarInteger(mdb_cmp(txn, dbi, &a, &b));
}

SEXP r_mdb_dcmp(SEXP r_txn, SEXP r_dbi, SEXP r_a, SEXP r_b) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_dbi dbi = r_mdb_get_dbi(r_dbi);
  MDB_val a, b;
  sexp_to_mdb_val(r_a, "a", &a);
  sexp_to_mdb_val(r_b, "b", &b);
  return ScalarInteger(mdb_dcmp(txn, dbi, &a, &b));
}

// TODO: Swap this out for an accumulator
int mdb_reader_list_callback(const char *msg, void *ctx) {
  REprintf(msg);
  return 0;
}

SEXP r_mdb_reader_list(SEXP r_env) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  mdb_reader_list(env, &mdb_reader_list_callback, NULL);
  return R_NilValue;
}

SEXP r_mdb_reader_check(SEXP r_env) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  int dead = 0;
  no_error(mdb_reader_check(env, &dead), "mdb_reader_check");
  return ScalarInteger(dead);
}

// --- wranglers ---
MDB_env * r_mdb_get_env(SEXP r_env, bool closed_error) {
  void * ret = r_pointer_addr(r_env, THOR_ENV, "env", closed_error);
  return (MDB_env*) ret;
}

MDB_txn * r_mdb_get_txn(SEXP r_txn, bool closed_error) {
  return (MDB_txn*) r_pointer_addr(r_txn, THOR_TXN, "txn", closed_error);
}

MDB_dbi r_mdb_get_dbi(SEXP r_dbi) {
  MDB_dbi* data = (MDB_dbi*) r_pointer_addr(r_dbi, THOR_DBI, "txn", true);
  return *data;
}

MDB_cursor * r_mdb_get_cursor(SEXP r_cursor, bool closed_error, bool orphaned) {
  SEXP r_orphaned = getAttrib(r_cursor, thor_cursor_orphan_name);
  if (closed_error && (orphaned == (r_orphaned == R_NilValue))) {
    if (orphaned) {
      Rf_error("Expected an orphaned cursor but recieved an open one");
    } else {
      Rf_error("Expected an open cursor but recieved an orphaned one");
    }
  }
  return (MDB_cursor*) r_pointer_addr(r_cursor, THOR_CURSOR, "cursor",
                                      closed_error);
}

void* r_pointer_addr(SEXP r_ptr, thor_ptr_type expected, const char * name,
                     bool closed_error) {
  if (r_ptr == R_NilValue) {
    Rf_error("%s has been cleaned up; can't use!", name);
  }
  if (TYPEOF(r_ptr) != EXTPTRSXP) {
    Rf_error("Expected an external pointer");
  }
  SEXP ptr_type = R_ExternalPtrTag(r_ptr);
  if (TYPEOF(ptr_type) != INTSXP ||
      length(ptr_type) != 1 ||
      INTEGER(ptr_type)[0] != expected) {
    Rf_error("Invalid pointer type recieved for %s", name);
  }
  void * contents = R_ExternalPtrAddr(r_ptr);
  if (closed_error && contents == NULL) {
    Rf_error("%s has been freed; can't use!", name);
  }
  return contents;
}

// --- wrappers ---
static SEXP r_mdb_env_wrap(MDB_env *env, bool opened) {
  SEXP ptr_type = PROTECT(ScalarInteger(THOR_ENV));
  SEXP ret = PROTECT(R_MakeExternalPtr(env, ptr_type, R_NilValue));
  R_RegisterCFinalizer(ret, r_mdb_env_finalize);
  setAttrib(ret, R_ClassSymbol, mkString("mdb_env"));
  UNPROTECT(2);
  return ret;
}

static SEXP r_mdb_txn_wrap(MDB_txn *txn) {
  SEXP ptr_type = PROTECT(ScalarInteger(THOR_TXN));
  SEXP ret = PROTECT(R_MakeExternalPtr(txn, ptr_type, R_NilValue));
  R_RegisterCFinalizer(ret, r_mdb_txn_finalize);
  setAttrib(ret, R_ClassSymbol, mkString("mdb_txn"));
  UNPROTECT(2);
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
  SEXP ptr_type = PROTECT(ScalarInteger(THOR_DBI));

  MDB_dbi * data = (MDB_dbi *)Calloc(1, MDB_dbi);
  *data = dbi;

  SEXP ret = PROTECT(R_MakeExternalPtr(data, ptr_type, R_NilValue));
  R_RegisterCFinalizer(ret, r_mdb_dbi_finalize);
  setAttrib(ret, R_ClassSymbol, mkString("mdb_dbi"));
  UNPROTECT(2);
  return ret;
}

static SEXP r_mdb_cursor_wrap(MDB_cursor *cursor) {
  SEXP ptr_type = PROTECT(ScalarInteger(THOR_CURSOR));
  SEXP ret = PROTECT(R_MakeExternalPtr(cursor, ptr_type, R_NilValue));
  R_RegisterCFinalizer(ret, r_mdb_cursor_finalize);
  setAttrib(ret, R_ClassSymbol, mkString("mdb_cursor"));
  UNPROTECT(2);
  return ret;
}

// --- finalizers ---

static void r_mdb_env_finalize(SEXP r_env) {
  MDB_env * env = r_mdb_get_env(r_env, false);
  if (env != NULL) {
    Rprintf("Cleaning environent\n");
    Rprintf("...closing environment\n");
    mdb_env_close(env);
    R_ClearExternalPtr(r_env);
  }
}

static void r_mdb_txn_finalize(SEXP r_txn) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, false);
  if (txn != NULL) {
    Rprintf("Cleaning transaction\n");
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
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, false, false);
  if (cursor != NULL) {
    Rprintf("Cleaning cursor\n");
    // These can definitely be garbage collected directly I think;
    // once they're out of scope we can't get it back.
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

SEXP mdb_missing_to_sexp(bool as_proxy, bool missing_is_error,
                         SEXP r_missing_value, SEXP r_key) {
  if (missing_is_error) {
    if (TYPEOF(r_key) == STRSXP) {
      Rf_error("Key '%s' not found in database",
               CHAR(STRING_ELT(r_key, 0)));
    } else {
      Rf_error("Key not found in database");
    }
  }
  return as_proxy ? R_NilValue : r_missing_value;
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
//
// TODO: push the transaction check in here; when we build a proxy we
// get the number of transactions and then push in the transaction as
// an argument here.  That then goes into the logic for resolving the
// proxy.
//
// TODO: Need a proper null proxy object too; that will be required
// for getting size treated the same way.
SEXP r_mdb_proxy_copy(SEXP r_proxy, SEXP r_as_raw) {
  thor_val_proxy *proxy = (thor_val_proxy*)R_ExternalPtrAddr(r_proxy);
  if (proxy == NULL) {
    Rf_error("proxy has been invalidated; can't use!");
  }
  return_as as_raw = to_return_as(r_as_raw);

  // This type detection bit here is the hard bit:
  if (as_raw == AS_ANY) {
    if (proxy->resolved[AS_STRING]) {
      as_raw = AS_STRING;
    } else if (proxy->data_contains_nul) {
      as_raw = AS_RAW;
    } else {
      proxy->data_contains_nul =
        is_raw_string(proxy->data, proxy->size, AS_ANY);
      as_raw = proxy->data_contains_nul ? AS_RAW : AS_STRING;
    }
  }

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
unsigned int sexp_to_mdb_flags(SEXP r_flags, thor_flag_group group_id) {
  int ret = 0;
  if (r_flags != R_NilValue) {
    // Here we could look at the class attribute instead but this will
    // be fine for now
    SEXP r_group_id = getAttrib(r_flags, thor_flag_group_id_name);
    if (r_group_id == R_NilValue) {
      Rf_error("mdb flags must be an mdb_flag object");
    }
    if (INTEGER(r_group_id)[0] != group_id) {
      // TODO: we should give more useful error messages here but that
      // involves a big boring switch statement.
      Rf_error("Wrong flag type");
    }
    size_t n_flags = length(r_flags);
    int *flags = INTEGER(r_flags);
    for (size_t i = 0; i < n_flags; ++i) {
      ret = ret | flags[i];
    }
  }
  return ret;
}

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
  SEXP r_group_id = getAttrib(r_cursor_op, thor_flag_group_id_name);
  if (r_group_id == R_NilValue) {
    Rf_error("cursor_op must be an mdb_flag object");
  }
  if (INTEGER(r_group_id)[0] != THOR_CURSOR_OP || length(r_cursor_op) != 1) {
    Rf_error("cursor_op must be an cursor_op object");
  }
  return INTEGER(r_cursor_op)[0];
}

// --- cursor_op ---
SEXP r_mdb_cursor_op() {
  int n = 19;
  SEXP ret = PROTECT(allocVector(INTSXP, n));
  SEXP nms = PROTECT(allocVector(STRSXP, n));

  INTEGER(ret)[0] = MDB_FIRST;
  SET_STRING_ELT(nms, 0, mkChar("FIRST"));
  INTEGER(ret)[1] = MDB_FIRST_DUP;
  SET_STRING_ELT(nms, 1, mkChar("FIRST_DUP"));
  INTEGER(ret)[2] = MDB_GET_BOTH;
  SET_STRING_ELT(nms, 2, mkChar("GET_BOTH"));
  INTEGER(ret)[3] = MDB_GET_BOTH_RANGE;
  SET_STRING_ELT(nms, 3, mkChar("GET_BOTH_RANGE"));
  INTEGER(ret)[4] = MDB_GET_CURRENT;
  SET_STRING_ELT(nms, 4, mkChar("GET_CURRENT"));
  INTEGER(ret)[5] = MDB_GET_MULTIPLE;
  SET_STRING_ELT(nms, 5, mkChar("GET_MULTIPLE"));
  INTEGER(ret)[6] = MDB_LAST;
  SET_STRING_ELT(nms, 6, mkChar("LAST"));
  INTEGER(ret)[7] = MDB_LAST_DUP;
  SET_STRING_ELT(nms, 7, mkChar("LAST_DUP"));
  INTEGER(ret)[8] = MDB_NEXT;
  SET_STRING_ELT(nms, 8, mkChar("NEXT"));
  INTEGER(ret)[9] = MDB_NEXT_DUP;
  SET_STRING_ELT(nms, 9, mkChar("NEXT_DUP"));
  INTEGER(ret)[10] = MDB_NEXT_MULTIPLE;
  SET_STRING_ELT(nms, 10, mkChar("NEXT_MULTIPLE"));
  INTEGER(ret)[11] = MDB_NEXT_NODUP;
  SET_STRING_ELT(nms, 11, mkChar("NEXT_NODUP"));
  INTEGER(ret)[12] = MDB_PREV;
  SET_STRING_ELT(nms, 12, mkChar("PREV"));
  INTEGER(ret)[13] = MDB_PREV_DUP;
  SET_STRING_ELT(nms, 13, mkChar("PREV_DUP"));
  INTEGER(ret)[14] = MDB_PREV_NODUP;
  SET_STRING_ELT(nms, 14, mkChar("PREV_NODUP"));
  INTEGER(ret)[15] = MDB_SET;
  SET_STRING_ELT(nms, 15, mkChar("SET"));
  INTEGER(ret)[16] = MDB_SET_KEY;
  SET_STRING_ELT(nms, 16, mkChar("SET_KEY"));
  INTEGER(ret)[17] = MDB_SET_RANGE;
  SET_STRING_ELT(nms, 17, mkChar("SET_RANGE"));
  INTEGER(ret)[18] = MDB_PREV_MULTIPLE;
  SET_STRING_ELT(nms, 18, mkChar("PREV_MULTIPLE"));

  setAttrib(ret, R_NamesSymbol, nms);
  setAttrib(ret, thor_flag_group_id_name, ScalarInteger(THOR_CURSOR_OP));
  UNPROTECT(2);
  return ret;
}

SEXP r_mdb_dbi_id(SEXP r_dbi) {
  MDB_dbi dbi = r_mdb_get_dbi(r_dbi);
  return ScalarInteger((int) dbi);
}
