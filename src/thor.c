#include "thor.h"
#include "util.h"

// These are symbols that we'll use here to avoid having to use
// `install()` at every use (following R-exts).  This is relatively
// straightforward here because of the global string cache so there is
// no GC to worry about.
SEXP thor_flag_group_id_name;
SEXP thor_env_txn_name;
SEXP thor_txn_cursor_name;
SEXP thor_txn_readonly_name;
SEXP thor_cursor_orphan_name;

void thor_init() {
  thor_flag_group_id_name = install("group_id");
  thor_env_txn_name = install("transaction");
  thor_txn_cursor_name = install("cursors");
  thor_txn_readonly_name = install("readonly");
  thor_cursor_orphan_name = install("orphan");
}

void thor_cleanup() {
}

static SEXP r_mdb_env_wrap(MDB_env *env, bool opened);
static SEXP r_mdb_txn_wrap(MDB_txn *txn, SEXP r_env);
static SEXP r_mdb_dbi_wrap(MDB_dbi dbi);
static SEXP r_mdb_cursor_wrap(MDB_cursor *cursor, SEXP r_txn);
static void r_mdb_env_finalize(SEXP r_env);
static void r_mdb_txn_finalize(SEXP r_txn);
static void r_mdb_dbi_finalize(SEXP r_dbi);
static void r_mdb_cursor_finalize(SEXP r_cursor);

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

SEXP r_mdb_env_open(SEXP r_env, SEXP r_path, SEXP r_flags) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  const char * path = scalar_character(r_path, "path");
  const unsigned int flags = sexp_to_mdb_flags(r_flags, THOR_FLAGS_ENV);
  const mdb_mode_t mode = 0644;

  int rc = mdb_env_open(env, path, flags, mode);
  if (rc != MDB_SUCCESS) {
    mdb_env_close(env);
    Rf_error("Error in mdb_env_open: %s", mdb_strerror(rc));
  }

  return R_NilValue;
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

SEXP r_mdb_env_set_flags(SEXP r_env, SEXP r_flags, SEXP r_set) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  const unsigned int flags = sexp_to_mdb_flags(r_flags, THOR_FLAGS_ENV);
  bool set = scalar_logical(r_set, "set");
  no_error(mdb_env_set_flags(env, flags, set), "mdb_env_set_flags");
  return R_NilValue;
}

SEXP r_mdb_env_get_flags(SEXP r_env) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  unsigned int flags = 0;
  no_error(mdb_env_get_flags(env, &flags), "mdb_env_get_flags");
  return ScalarInteger(flags);
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
SEXP r_mdb_txn_begin(SEXP r_env, SEXP r_parent, SEXP r_flags) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  MDB_txn * parent =
    r_parent == R_NilValue ? NULL : r_mdb_get_txn(r_parent, true);
  const unsigned int
    flags = sexp_to_mdb_flags(r_flags, THOR_FLAGS_TXN);

  MDB_txn *txn;
  no_error(mdb_txn_begin(env, parent, flags, &txn), "mdb_txn_begin");
  SEXP r_txn = PROTECT(r_mdb_txn_wrap(txn, r_env));

  setAttrib(r_env, thor_env_txn_name, r_txn);
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
  cleanup_txn(r_txn);
  return R_NilValue;
}

SEXP r_mdb_txn_abort(SEXP r_txn) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  mdb_txn_abort(txn);
  cleanup_txn(r_txn);
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
  cleanup_txn_cursors(r_txn);
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
    sexp_to_flag(r_reversekey, MDB_REVERSEKEY, "reversekey") |
    sexp_to_flag(r_dupsort, MDB_DUPSORT, "dupsort") |
    sexp_to_flag(r_create, MDB_CREATE, "create");

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
SEXP r_mdb_get(SEXP r_txn, SEXP r_dbi, SEXP r_key) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_dbi dbi = r_mdb_get_dbi(r_dbi);
  MDB_val key, data;
  sexp_to_mdb_val(r_key, "key", &key);
  no_error(mdb_get(txn, dbi, &key, &data), "mdb_get");
  return mdb_val_to_sexp(&data);
}

SEXP r_mdb_put(SEXP r_txn, SEXP r_dbi, SEXP r_key, SEXP r_data, SEXP r_flags) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_dbi dbi = r_mdb_get_dbi(r_dbi);
  MDB_val key, data;
  const unsigned int flags = sexp_to_mdb_flags(r_flags, THOR_FLAGS_PUT);
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
  SEXP r_cursor = PROTECT(r_mdb_cursor_wrap(cursor, r_txn));
  setAttrib(r_txn, thor_txn_cursor_name,
            CONS(r_cursor, getAttrib(r_txn, thor_txn_cursor_name)));
  UNPROTECT(1);
  return r_cursor;
}

SEXP r_mdb_cursor_close(SEXP r_cursor) {
  SEXP r_txn = R_ExternalPtrProtected(r_cursor);
  cleanup_cursor(r_cursor, r_txn);
  return R_NilValue;
}

SEXP r_mdb_cursor_renew(SEXP r_txn, SEXP r_cursor) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, true, true);
  mdb_cursor_renew(txn, cursor);
  // Register the cursor with the transaction:
  R_SetExternalPtrProtected(r_cursor, r_txn);
  setAttrib(r_cursor, thor_cursor_orphan_name, R_NilValue);
  setAttrib(r_txn, thor_txn_cursor_name,
            CONS(r_cursor, getAttrib(r_txn, thor_txn_cursor_name)));
  return R_NilValue;
}

SEXP r_mdb_cursor_txn(SEXP r_cursor) {
  // This first bit is called for the side effect of making sure that
  // we have a valid cursor.
  r_mdb_get_cursor(r_cursor, true, false);
  // The other option here is we create a *second* txn object and wrap
  // that up but I prefer this way (even if it sidesteps the mdb API)
  return R_ExternalPtrProtected(r_cursor);
}

SEXP r_mdb_cursor_dbi(SEXP r_cursor) {
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, true, false);
  MDB_dbi dbi = mdb_cursor_dbi(cursor);
  return r_mdb_dbi_wrap(dbi);
}

SEXP r_mdb_cursor_get(SEXP r_cursor, SEXP r_key, SEXP r_cursor_op) {
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, true, false);
  MDB_val key, data;
  if (r_key != R_NilValue) {
    sexp_to_mdb_val(r_key, "key", &key);
  }
  MDB_cursor_op cursor_op = sexp_to_cursor_op(r_cursor_op);
  // MDB_NOTFOUND should not necessarily be an error
  no_error(mdb_cursor_get(cursor, &key, &data, cursor_op), "mdb_cursor_get");
  // some trickery here will be useful to send a proxy object back for
  // the data rather than the full data.  Or we could return something
  // like a list.  For now, that's what I'm going to do; this would be
  // the *maximal* return object.
  SEXP ret = PROTECT(allocVector(VECSXP, 2));
  // could avoid a key here if we do a quick comparison of the input key
  // if r_key != R_Nilvalue &&
  //    key.len == r_key.len &&
  //    key.data == r_key.data
  // then
  //    SET_VECTOR_ELT(ret, 1, r_key);
  // else
  //    ...as below
  SET_VECTOR_ELT(ret, 0, mdb_val_to_sexp(&key));
  SET_VECTOR_ELT(ret, 1, mdb_val_to_sexp(&data));
  UNPROTECT(1);
  return ret;
}

SEXP r_mdb_cursor_put(SEXP r_cursor, SEXP r_key, SEXP r_data, SEXP r_flags) {
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, true, false);
  MDB_val key, data;
  sexp_to_mdb_val(r_key, "key", &key);
  sexp_to_mdb_val(r_data, "data", &data);
  const unsigned int flags = sexp_to_mdb_flags(r_flags, THOR_FLAGS_PUT);
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

static SEXP r_mdb_txn_wrap(MDB_txn *txn, SEXP r_env) {
  // The options here for getting the GC right are we can
  //
  // - add the env to the tag of the txn - this takes care of keeping
  //   the transaction alive in the case of accidental deletion of the
  //   handle, so we'll do that.
  // - add the txn to something in the environment so that when the
  //   environment is *forceably* closed we can abort all
  //   transactions - this is not done yet.
  SEXP ptr_type = PROTECT(ScalarInteger(THOR_TXN));
  SEXP ret = PROTECT(R_MakeExternalPtr(txn, ptr_type, r_env));
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

static SEXP r_mdb_cursor_wrap(MDB_cursor *cursor, SEXP r_txn) {
  SEXP ptr_type = PROTECT(ScalarInteger(THOR_CURSOR));
  SEXP ret = PROTECT(R_MakeExternalPtr(cursor, ptr_type, r_txn));
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
    // Some care is needed here I think
    // mdb_env_close(env);
    SEXP r_txn = getAttrib(r_env, thor_env_txn_name);
    if (r_txn != R_NilValue) {
      Rprintf("...first to clean up transaction\n");
      r_mdb_txn_finalize(r_txn);
    }
    Rprintf("...closing environment\n");
    mdb_env_close(env);
    R_ClearExternalPtr(r_env);
  }
}

static void r_mdb_txn_finalize(SEXP r_txn) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, false);
  if (txn != NULL) {
    Rprintf("Cleaning transaction\n");
    // TODO: deal with live cursors here
    Rprintf("...aborting live transaction\n");
    mdb_txn_abort(txn);
    SEXP r_env = R_ExternalPtrProtected(r_txn);
    if (getAttrib(r_env, thor_env_txn_name) == r_txn) {
      Rprintf("...unsetting env/txn link\n");
      setAttrib(r_env, thor_env_txn_name, R_NilValue);
    }
    R_ClearExternalPtr(r_txn);
    R_SetExternalPtrProtected(r_txn, R_NilValue);
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

// --- other ---
void sexp_to_mdb_val(SEXP r_x, const char *name, MDB_val *x) {
  // For now we'll just go with a *pure* string interface and worry
  // about a more complete interface later.  There are some fun things
  // that can be done that are more interesting than string keys (most
  // obviously raw keys but also read-only mmaped return values,
  // direct serialising of atomic types, protocol buffers, etc).
  if (TYPEOF(r_x) != STRSXP || length(r_x) != 1) {
    Rf_error("Expected a scalar character for %s", name);
  }
  SEXP r_c = STRING_ELT(r_x, 0);

  // TODO: Can we get away here without a copy?  I believe that mdb
  // will not modify or free these, and expects the same of us on
  // return.  A more subtle issue though is that if the transaction
  // does not complete before 'x' is garbage collected (which could
  // happen especially under gctorture) then by the time the data is
  // added there will not be any data to add and we'll put junk in.
  // So we'll at least need a "safe" mode here that does do the
  // allocation and copy but also promise to clean things up later
  // (perhaps - that might not be the case)
  x->mv_data = (void*) CHAR(r_c);
  x->mv_size = length(r_c);
}

SEXP mdb_val_to_sexp(MDB_val *x) {
  SEXP ret = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(ret, 0, mkCharLen(x->mv_data, x->mv_size));
  UNPROTECT(1);
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
SEXP r_mdb_flags_env() {
  int n = 11;
  SEXP ret = PROTECT(allocVector(INTSXP, n));
  SEXP nms = PROTECT(allocVector(STRSXP, n));

  INTEGER(ret)[0] = MDB_FIXEDMAP;
  SET_STRING_ELT(nms, 0, mkChar("FIXEDMAP"));
  INTEGER(ret)[1] = MDB_NOSUBDIR;
  SET_STRING_ELT(nms, 1, mkChar("NOSUBDIR"));
  INTEGER(ret)[2] = MDB_RDONLY;
  SET_STRING_ELT(nms, 2, mkChar("RDONLY"));
  INTEGER(ret)[3] = MDB_WRITEMAP;
  SET_STRING_ELT(nms, 3, mkChar("WRITEMAP"));
  INTEGER(ret)[4] = MDB_NOMETASYNC;
  SET_STRING_ELT(nms, 4, mkChar("NOMETASYNC"));
  INTEGER(ret)[5] = MDB_NOSYNC;
  SET_STRING_ELT(nms, 5, mkChar("NOSYNC"));
  INTEGER(ret)[6] = MDB_MAPASYNC;
  SET_STRING_ELT(nms, 6, mkChar("MAPASYNC"));
  INTEGER(ret)[7] = MDB_NOTLS;
  SET_STRING_ELT(nms, 7, mkChar("NOTLS"));
  INTEGER(ret)[8] = MDB_NOLOCK;
  SET_STRING_ELT(nms, 8, mkChar("NOLOCK"));
  INTEGER(ret)[9] = MDB_NORDAHEAD;
  SET_STRING_ELT(nms, 9, mkChar("NORDAHEAD"));
  INTEGER(ret)[10] = MDB_NOMEMINIT;
  SET_STRING_ELT(nms, 10, mkChar("NOMEMINIT"));

  setAttrib(ret, R_NamesSymbol, nms);
  setAttrib(ret, thor_flag_group_id_name, ScalarInteger(THOR_FLAGS_ENV));
  UNPROTECT(2);
  return ret;
}

// mdb_dbi_open:
SEXP r_mdb_flags_dbi() {
  int n = 7;
  SEXP ret = PROTECT(allocVector(INTSXP, n));
  SEXP nms = PROTECT(allocVector(STRSXP, n));

  // mdb_dbi_open:
  INTEGER(ret)[0] = MDB_REVERSEKEY;
  SET_STRING_ELT(nms, 0, mkChar("REVERSEKEY"));
  INTEGER(ret)[1] = MDB_DUPSORT;
  SET_STRING_ELT(nms, 1, mkChar("DUPSORT"));
  INTEGER(ret)[2] = MDB_INTEGERKEY;
  SET_STRING_ELT(nms, 2, mkChar("INTEGERKEY"));
  INTEGER(ret)[3] = MDB_DUPFIXED;
  SET_STRING_ELT(nms, 3, mkChar("DUPFIXED"));
  INTEGER(ret)[4] = MDB_INTEGERDUP;
  SET_STRING_ELT(nms, 4, mkChar("INTEGERDUP"));
  INTEGER(ret)[5] = MDB_REVERSEDUP;
  SET_STRING_ELT(nms, 5, mkChar("REVERSEDUP"));
  INTEGER(ret)[6] = MDB_CREATE;
  SET_STRING_ELT(nms, 6, mkChar("CREATE"));

  setAttrib(ret, R_NamesSymbol, nms);
  setAttrib(ret, thor_flag_group_id_name, ScalarInteger(THOR_FLAGS_DBI));
  UNPROTECT(2);
  return ret;
}

SEXP r_mdb_flags_txn() {
  int n = 3;
  SEXP ret = PROTECT(allocVector(INTSXP, n));
  SEXP nms = PROTECT(allocVector(STRSXP, n));

  // mdb_dbi_open:
  INTEGER(ret)[0] = MDB_RDONLY;
  SET_STRING_ELT(nms, 0, mkChar("RDONLY"));
  INTEGER(ret)[1] = MDB_NOSYNC;
  SET_STRING_ELT(nms, 1, mkChar("NOSYNC"));
  INTEGER(ret)[2] = MDB_NOMETASYNC;
  SET_STRING_ELT(nms, 2, mkChar("NOMETASYNC"));

  setAttrib(ret, R_NamesSymbol, nms);
  setAttrib(ret, thor_flag_group_id_name, ScalarInteger(THOR_FLAGS_TXN));
  UNPROTECT(2);
  return ret;
}

SEXP r_mdb_flags_put() {
  int n = 7;
  SEXP ret = PROTECT(allocVector(INTSXP, n));
  SEXP nms = PROTECT(allocVector(STRSXP, n));

  INTEGER(ret)[0] = MDB_NOOVERWRITE;
  SET_STRING_ELT(nms, 0, mkChar("NOOVERWRITE"));
  INTEGER(ret)[1] = MDB_NODUPDATA;
  SET_STRING_ELT(nms, 1, mkChar("NODUPDATA"));
  INTEGER(ret)[2] = MDB_CURRENT;
  SET_STRING_ELT(nms, 2, mkChar("CURRENT"));
  INTEGER(ret)[3] = MDB_RESERVE;
  SET_STRING_ELT(nms, 3, mkChar("RESERVE"));
  INTEGER(ret)[4] = MDB_APPEND;
  SET_STRING_ELT(nms, 4, mkChar("APPEND"));
  INTEGER(ret)[5] = MDB_APPENDDUP;
  SET_STRING_ELT(nms, 5, mkChar("APPENDDUP"));
  INTEGER(ret)[6] = MDB_MULTIPLE;
  SET_STRING_ELT(nms, 6, mkChar("MULTIPLE"));

  setAttrib(ret, R_NamesSymbol, nms);
  setAttrib(ret, thor_flag_group_id_name, ScalarInteger(THOR_FLAGS_PUT));
  UNPROTECT(2);
  return ret;
}

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

unsigned int sexp_to_flag(SEXP r_x, unsigned int if_set, const char *name) {
  if (r_x == R_NilValue) {
    return 0;
  } else {
    return scalar_logical(r_x, name) ? if_set : 0;
  }
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

// This is fine for now, but better would be to render these good to
// renew
void cleanup_txn_cursors(SEXP r_txn) {
  SEXP r_cursors = getAttrib(r_txn, thor_txn_cursor_name);
  bool txn_readonly =
    (bool)INTEGER(getAttrib(r_txn, thor_txn_readonly_name))[0];
  if (txn_readonly) {
    setAttrib(r_txn, thor_txn_cursor_name, R_NilValue);
    while (r_cursors != R_NilValue) {
      SEXP r_cursor = CAR(r_cursors);
      MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, false, false);
      if (cursor) {
        mdb_cursor_close(cursor);
      }
      R_SetExternalPtrProtected(r_cursor, R_NilValue);
      setAttrib(r_cursor, thor_cursor_orphan_name, ScalarLogical(true));
      // And continue...
      r_cursors = CDR(r_cursors);
    }
  } else {
    while (r_cursors != R_NilValue) {
      r_mdb_cursor_finalize(CAR(r_cursors));
      r_cursors = CDR(r_cursors);
    }
  }
}

void cleanup_cursor(SEXP r_cursor, SEXP r_txn) {
  // Stop protecting the transaction:
  R_SetExternalPtrProtected(r_cursor, R_NilValue);
  // Remove cursor from the transaction itself
  setAttrib(r_txn, thor_txn_cursor_name,
            pairlist_drop(getAttrib(r_txn, thor_txn_cursor_name), r_cursor));
  // Close the cursor itself:
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, true, false);
  mdb_cursor_close(cursor);
  // Mark as closed
  setAttrib(r_cursor, thor_cursor_orphan_name, ScalarLogical(true));
}

void cleanup_txn(SEXP r_txn) {
  R_ClearExternalPtr(r_txn);
  cleanup_txn_cursors(r_txn);
  SEXP r_env = R_ExternalPtrProtected(r_txn);
  R_SetExternalPtrProtected(r_txn, R_NilValue);
  setAttrib(r_env, thor_env_txn_name, R_NilValue);
}

SEXP  r_mdb_dbi_id(SEXP r_dbi) {
  MDB_dbi dbi = r_mdb_get_dbi(r_dbi);
  return ScalarInteger((int) dbi);
}
