#include "thor.h"
#include "util.h"

// TODO: consider adding a tag to every ptr type so that we can do
// some basic type checking.  Options there are a newly allocated
// (say) integer/enum type with the cost of one allocation.  The other
// way would be to have some package-level singleton SEXP objects that
// we can stick in as the tag.

void thor_init() {
}

void thor_cleanup() {
}

static SEXP r_mdb_env_wrap(MDB_env *env);
static SEXP r_mdb_txn_wrap(MDB_txn *txn);
static SEXP r_mdb_dbi_wrap(MDB_dbi *dbi);
static SEXP r_mdb_cursor_wrap(MDB_cursor *cursor);
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
  return r_mdb_env_wrap(env);
}

SEXP r_mdb_env_open(SEXP r_env, SEXP r_path, SEXP r_flags) {
  bool new_env = r_env == R_NilValue;
  if (new_env) {
    r_env = PROTECT(r_mdb_env_create());
  }
  MDB_env * env = r_mdb_get_env(r_env, true);
  const char * path = scalar_character(r_path, "path");
  // TODO: more work here
  const int flags = sexp_to_mdb_flags(r_flags);;
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

// Transactions:
SEXP r_mdb_txn_begin(SEXP r_env, SEXP r_parent, SEXP r_flags) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  MDB_txn * parent =
    r_parent == R_NilValue ? NULL : r_mdb_get_txn(r_parent, true);
  const int flags = sexp_to_mdb_flags(r_flags);;

  MDB_txn *txn;
  no_error(mdb_txn_begin(env, parent, flags, &txn), "mdb_txn_begin");
  return r_mdb_txn_wrap(txn);
}

SEXP r_mdb_txn_env(SEXP r_txn) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_env * env = mdb_txn_env(txn);
  return r_mdb_env_wrap(env);
}

SEXP r_mdb_txn_id(SEXP r_txn) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  return ScalarInteger(mdb_txn_id(txn));
}

SEXP r_mdb_txn_commit(SEXP r_txn) {
  // TODO: there is some serious work that might need to be done here
  // about cleaning up the txn cursor - see the docs.  It looks like
  // we're going to need to hold onto something with the transaction
  // that says whether it has been destroyed (via commit/abort) or
  // we'll be pushing too much onto the user.
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  no_error(mdb_txn_commit(txn), "mdb_txn_commit");
  return R_NilValue;
}

SEXP r_mdb_txn_abort(SEXP r_txn) {
  // TODO: there is some serious work that might need to be done here
  // about cleaning up the txn cursor - see the docs
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  mdb_txn_abort(txn);
  return R_NilValue;
}

SEXP r_mdb_txn_reset(SEXP r_txn) {
  // TODO: there is some serious work that might need to be done here
  // about cleaning up the txn cursor - see the docs
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  mdb_txn_reset(txn);
  return R_NilValue;
}

SEXP r_mdb_txn_renew(SEXP r_txn) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  no_error(mdb_txn_renew(txn), "mdb_txn_renew");
  return R_NilValue;
}

SEXP r_mdb_dbi_open(SEXP r_txn, SEXP r_name, SEXP r_flags) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  const char * name =
    r_name == R_NilValue ? NULL : scalar_character(r_name, "name");
  const int flags = sexp_to_mdb_flags(r_flags);;

  MDB_dbi * dbi = (MDB_dbi *)Calloc(1, MDB_dbi);
  no_error(mdb_dbi_open(txn, name, flags, dbi), "mdb_dbi_open");

  return r_mdb_dbi_wrap(dbi);
}

// "Normally unnecessary. Use with care"
SEXP r_mdb_dbi_close(SEXP r_env, SEXP r_dbi) {
  MDB_env * env = r_mdb_get_env(r_env, true);
  MDB_dbi * dbi = r_mdb_get_dbi(r_dbi, true);
  mdb_dbi_close(env, *dbi);
  return R_NilValue;
}

// --- use the database ---
SEXP r_mdb_get(SEXP r_txn, SEXP r_dbi, SEXP r_key) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_dbi * dbi = r_mdb_get_dbi(r_dbi, true);
  MDB_val key, data;
  sexp_to_mdb_val(r_key, "key", &key);
  no_error(mdb_get(txn, *dbi, &key, &data), "mdb_get");
  return mdb_val_to_sexp(&data);
}

SEXP r_mdb_put(SEXP r_txn, SEXP r_dbi, SEXP r_key, SEXP r_data, SEXP r_flags) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_dbi * dbi = r_mdb_get_dbi(r_dbi, true);
  MDB_val key, data;
  const int flags = sexp_to_mdb_flags(r_flags);
  sexp_to_mdb_val(r_key, "key", &key);
  sexp_to_mdb_val(r_data, "data", &data);
  no_error(mdb_put(txn, *dbi, &key, &data, flags), "mdb_put");
  return R_NilValue;
}

SEXP r_mdb_del(SEXP r_txn, SEXP r_dbi, SEXP r_key, SEXP r_data) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_dbi * dbi = r_mdb_get_dbi(r_dbi, true);
  MDB_val key, data;
  sexp_to_mdb_val(r_key, "key", &key);
  if (r_data == NULL) {
    data.mv_size = 0;
    data.mv_data = "";
  } else {
    sexp_to_mdb_val(r_data, "data", &data);
  }
  no_error(mdb_del(txn, *dbi, &key, &data), "mdb_del");
  return R_NilValue;
}

// --- cursors ---
SEXP r_mdb_cursor_open(SEXP r_txn, SEXP r_dbi) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_dbi * dbi = r_mdb_get_dbi(r_dbi, true);
  MDB_cursor *cursor;
  no_error(mdb_cursor_open(txn, *dbi, &cursor), "mdb_cursor_open");
  return r_mdb_cursor_wrap(cursor);
}

SEXP r_mdb_cursor_close(SEXP r_cursor) {
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, true);
  mdb_cursor_close(cursor);
  return R_NilValue;
}

SEXP r_mdb_cursor_renew(SEXP r_txn, SEXP r_cursor) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, true);
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, true);
  mdb_cursor_renew(txn, cursor);
  return R_NilValue;
}

// SEXP r_mdb_cursor_transaction(SEXP r_cursor);
// SEXP r_mdb_cursor_dbi(SEXP r_cursor);
SEXP r_mdb_cursor_get(SEXP r_cursor, SEXP r_key, SEXP r_cursor_op) {
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, true);
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
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, true);
  MDB_val key, data;
  sexp_to_mdb_val(r_key, "key", &key);
  sexp_to_mdb_val(r_data, "data", &data);
  const int flags = sexp_to_mdb_flags(r_flags);
  no_error(mdb_cursor_put(cursor, &key, &data, flags), "mdb_cursor_put");
  return R_NilValue;
}

SEXP r_mdb_cursor_del(SEXP r_cursor, SEXP r_flags) {
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, true);
  const int flags = sexp_to_mdb_flags(r_flags);
  no_error(mdb_cursor_del(cursor, flags), "mdb_cursor_del");
  return R_NilValue;
}

SEXP r_mdb_cursor_count(SEXP r_cursor) {
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, true);
  mdb_size_t count;
  no_error(mdb_cursor_count(cursor, &count), "mdb_cursor_count");
  return ScalarInteger(count);
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

MDB_cursor * r_mdb_get_cursor(SEXP r_cursor, bool closed_error) {
  if (TYPEOF(r_cursor) != EXTPTRSXP) {
    Rf_error("Expected an external pointer");
  }
  MDB_cursor* cursor = (MDB_cursor*) R_ExternalPtrAddr(r_cursor);
  if (!cursor && closed_error) {
    Rf_error("mdb cursor is not open; can't connect");
  }
  return (MDB_cursor*) cursor;
}

MDB_dbi * r_mdb_get_dbi(SEXP r_dbi, bool closed_error) {
  if (TYPEOF(r_dbi) != EXTPTRSXP) {
    Rf_error("Expected an external pointer");
  }
  MDB_dbi* dbi = (MDB_dbi*) R_ExternalPtrAddr(r_dbi);
  if (!dbi && closed_error) {
    Rf_error("mdb dbi is not open; can't connect");
  }
  return (MDB_dbi*) dbi;
}

// --- wrappers ---
static SEXP r_mdb_env_wrap(MDB_env *env) {
  SEXP ret = PROTECT(R_MakeExternalPtr(env, R_NilValue, R_NilValue));
  R_RegisterCFinalizer(ret, r_mdb_env_finalize);
  setAttrib(ret, R_ClassSymbol, mkString("mdb_env"));
  UNPROTECT(1);
  return ret;
}

static SEXP r_mdb_txn_wrap(MDB_txn *txn) {
  // The options here for getting the GC right are we can
  //
  // - add the env to the tag of the txn - this takes care of keeping
  //   the transaction alive in the case of accidental deletion of the
  //   handle, so we'll do that.
  // - add the txn to something in the environment so that when the
  //   environment is *forceably* closed we can abort all
  //   transactions - this is not done yet.
  SEXP ret = PROTECT(R_MakeExternalPtr(txn, R_NilValue, R_NilValue));
  R_RegisterCFinalizer(ret, r_mdb_txn_finalize);
  setAttrib(ret, R_ClassSymbol, mkString("mdb_txn"));
  UNPROTECT(1);
  return ret;
}

static SEXP r_mdb_dbi_wrap(MDB_dbi *dbi) {
  // The options here for getting the GC right are we can
  //
  // - add the txn to the tag of the dbi - this takes care of keeping
  //   the transaction alive in the case of accidental deletion of the
  //   handle, so we'll do that.
  // - add the dbi to something in the txn so that when the
  //   transaction is *forceably* closed we can abort all
  //   connections - this is not done yet.
  SEXP ret = PROTECT(R_MakeExternalPtr(dbi, R_NilValue, R_NilValue));
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
    Rprintf("Cleaning environent\n");
    // Some care is needed here I think
    // mdb_env_close(env);
    R_ClearExternalPtr(r_env);
  }
}

static void r_mdb_dbi_finalize(SEXP r_dbi) {
  MDB_dbi * dbi = r_mdb_get_dbi(r_dbi, false);
  if (dbi != NULL) {
    Rprintf("Cleaning handle\n");
    // mdb_dbi_close(dbi); --- needed?  Docs suggest not really.  And
    // if closed then cursors are at risk.
    // Free(dbi);
    R_ClearExternalPtr(r_dbi);
  }
}

static void r_mdb_txn_finalize(SEXP r_txn) {
  MDB_txn * txn = r_mdb_get_txn(r_txn, false);
  if (txn != NULL) {
    Rprintf("Cleaning transaction\n");
    // This is not *necessarily* the right thing to do; it would
    // really depend if there are any live cursors.
    // mdb_txn_abort(txn);
    R_ClearExternalPtr(r_txn);
  }
}

static void r_mdb_cursor_finalize(SEXP r_cursor) {
  MDB_cursor * cursor = r_mdb_get_cursor(r_cursor, false);
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

// Flags
SEXP r_mdb_flags_env() {
  int n = 11;
  SEXP ret = PROTECT(allocVector(INTSXP, n));
  SEXP nms = PROTECT(allocVector(STRSXP, n));

  INTEGER(ret)[0] = MDB_FIXEDMAP;
  SET_STRING_ELT(nms, 0, mkChar("MDB_FIXEDMAP"));
  INTEGER(ret)[1] = MDB_NOSUBDIR;
  SET_STRING_ELT(nms, 1, mkChar("MDB_NOSUBDIR"));
  INTEGER(ret)[2] = MDB_RDONLY;
  SET_STRING_ELT(nms, 2, mkChar("MDB_RDONLY"));
  INTEGER(ret)[3] = MDB_WRITEMAP;
  SET_STRING_ELT(nms, 3, mkChar("MDB_WRITEMAP"));
  INTEGER(ret)[4] = MDB_NOMETASYNC;
  SET_STRING_ELT(nms, 4, mkChar("MDB_NOMETASYNC"));
  INTEGER(ret)[5] = MDB_NOSYNC;
  SET_STRING_ELT(nms, 5, mkChar("MDB_NOSYNC"));
  INTEGER(ret)[6] = MDB_MAPASYNC;
  SET_STRING_ELT(nms, 6, mkChar("MDB_MAPASYNC"));
  INTEGER(ret)[7] = MDB_NOTLS;
  SET_STRING_ELT(nms, 7, mkChar("MDB_NOTLS"));
  INTEGER(ret)[8] = MDB_NOLOCK;
  SET_STRING_ELT(nms, 8, mkChar("MDB_NOLOCK"));
  INTEGER(ret)[9] = MDB_NORDAHEAD;
  SET_STRING_ELT(nms, 9, mkChar("MDB_NORDAHEAD"));
  INTEGER(ret)[10] = MDB_NOMEMINIT;
  SET_STRING_ELT(nms, 10, mkChar("MDB_NOMEMINIT"));

  setAttrib(ret, R_NamesSymbol, nms);
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
  SET_STRING_ELT(nms, 0, mkChar("MDB_REVERSEKEY"));
  INTEGER(ret)[1] = MDB_DUPSORT;
  SET_STRING_ELT(nms, 1, mkChar("MDB_DUPSORT"));
  INTEGER(ret)[2] = MDB_INTEGERKEY;
  SET_STRING_ELT(nms, 2, mkChar("MDB_INTEGERKEY"));
  INTEGER(ret)[3] = MDB_DUPFIXED;
  SET_STRING_ELT(nms, 3, mkChar("MDB_DUPFIXED"));
  INTEGER(ret)[4] = MDB_INTEGERDUP;
  SET_STRING_ELT(nms, 4, mkChar("MDB_INTEGERDUP"));
  INTEGER(ret)[5] = MDB_REVERSEDUP;
  SET_STRING_ELT(nms, 5, mkChar("MDB_REVERSEDUP"));
  INTEGER(ret)[6] = MDB_CREATE;
  SET_STRING_ELT(nms, 6, mkChar("MDB_CREATE"));

  setAttrib(ret, R_NamesSymbol, nms);
  UNPROTECT(2);
  return ret;
}

SEXP r_mdb_flags_write() {
  int n = 7;
  SEXP ret = PROTECT(allocVector(INTSXP, n));
  SEXP nms = PROTECT(allocVector(STRSXP, n));

  INTEGER(ret)[0] = MDB_NOOVERWRITE;
  SET_STRING_ELT(nms, 0, mkChar("MDB_NOOVERWRITE"));
  INTEGER(ret)[1] = MDB_NODUPDATA;
  SET_STRING_ELT(nms, 1, mkChar("MDB_NODUPDATA"));
  INTEGER(ret)[2] = MDB_CURRENT;
  SET_STRING_ELT(nms, 2, mkChar("MDB_CURRENT"));
  INTEGER(ret)[3] = MDB_RESERVE;
  SET_STRING_ELT(nms, 3, mkChar("MDB_RESERVE"));
  INTEGER(ret)[4] = MDB_APPEND;
  SET_STRING_ELT(nms, 4, mkChar("MDB_APPEND"));
  INTEGER(ret)[5] = MDB_APPENDDUP;
  SET_STRING_ELT(nms, 5, mkChar("MDB_APPENDDUP"));
  INTEGER(ret)[6] = MDB_MULTIPLE;
  SET_STRING_ELT(nms, 6, mkChar("MDB_MULTIPLE"));

  setAttrib(ret, R_NamesSymbol, nms);
  UNPROTECT(2);
  return ret;
}

SEXP r_mdb_flags_copy() {
  int n = 1;
  SEXP ret = PROTECT(allocVector(INTSXP, n));
  SEXP nms = PROTECT(allocVector(STRSXP, n));

  INTEGER(ret)[0] = MDB_CP_COMPACT;
  SET_STRING_ELT(nms, 0, mkChar("MDB_CP_COMPACT"));

  setAttrib(ret, R_NamesSymbol, nms);
  UNPROTECT(2);
  return ret;
}

int sexp_to_mdb_flags(SEXP r_flags) {
  int ret = 0;
  if (r_flags != R_NilValue) {
    // Here we could look at the class attribute instead but this will
    // be fine for now
    if (TYPEOF(r_flags) != INTSXP) {
      Rf_error("mdb flags must be an mdb_flag object");
    }
    size_t n_flags = length(r_flags);
    int *flags = INTEGER(r_flags);
    for (size_t i = 0; i < n_flags; ++i) {
      ret = ret | flags[i];
    }
  }
  return ret;
}

MDB_cursor_op sexp_to_cursor_op(SEXP r_cursor_op) {
  // Here we could look at the class attribute instead but this will
  // be fine for now
  if (TYPEOF(r_cursor_op) != INTSXP || length(r_cursor_op) != 1) {
    // TODO: this is not a great reflection of reality:
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
  SET_STRING_ELT(nms, 0, mkChar("MDB_FIRST"));
  INTEGER(ret)[1] = MDB_FIRST_DUP;
  SET_STRING_ELT(nms, 1, mkChar("MDB_FIRST_DUP"));
  INTEGER(ret)[2] = MDB_GET_BOTH;
  SET_STRING_ELT(nms, 2, mkChar("MDB_GET_BOTH"));
  INTEGER(ret)[3] = MDB_GET_BOTH_RANGE;
  SET_STRING_ELT(nms, 3, mkChar("MDB_GET_BOTH_RANGE"));
  INTEGER(ret)[4] = MDB_GET_CURRENT;
  SET_STRING_ELT(nms, 4, mkChar("MDB_GET_CURRENT"));
  INTEGER(ret)[5] = MDB_GET_MULTIPLE;
  SET_STRING_ELT(nms, 5, mkChar("MDB_GET_MULTIPLE"));
  INTEGER(ret)[6] = MDB_LAST;
  SET_STRING_ELT(nms, 6, mkChar("MDB_LAST"));
  INTEGER(ret)[7] = MDB_LAST_DUP;
  SET_STRING_ELT(nms, 7, mkChar("MDB_LAST_DUP"));
  INTEGER(ret)[8] = MDB_NEXT;
  SET_STRING_ELT(nms, 8, mkChar("MDB_NEXT"));
  INTEGER(ret)[9] = MDB_NEXT_DUP;
  SET_STRING_ELT(nms, 9, mkChar("MDB_NEXT_DUP"));
  INTEGER(ret)[10] = MDB_NEXT_MULTIPLE;
  SET_STRING_ELT(nms, 10, mkChar("MDB_NEXT_MULTIPLE"));
  INTEGER(ret)[11] = MDB_NEXT_NODUP;
  SET_STRING_ELT(nms, 11, mkChar("MDB_NEXT_NODUP"));
  INTEGER(ret)[12] = MDB_PREV;
  SET_STRING_ELT(nms, 12, mkChar("MDB_PREV"));
  INTEGER(ret)[13] = MDB_PREV_DUP;
  SET_STRING_ELT(nms, 13, mkChar("MDB_PREV_DUP"));
  INTEGER(ret)[14] = MDB_PREV_NODUP;
  SET_STRING_ELT(nms, 14, mkChar("MDB_PREV_NODUP"));
  INTEGER(ret)[15] = MDB_SET;
  SET_STRING_ELT(nms, 15, mkChar("MDB_SET"));
  INTEGER(ret)[16] = MDB_SET_KEY;
  SET_STRING_ELT(nms, 16, mkChar("MDB_SET_KEY"));
  INTEGER(ret)[17] = MDB_SET_RANGE;
  SET_STRING_ELT(nms, 17, mkChar("MDB_SET_RANGE"));
  INTEGER(ret)[18] = MDB_PREV_MULTIPLE;
  SET_STRING_ELT(nms, 18, mkChar("MDB_PREV_MULTIPLE"));

  setAttrib(ret, R_NamesSymbol, nms);
  UNPROTECT(2);
  return ret;
}
