#include "util.h"

void no_error(int x, const char* str) {
  if (x != MDB_SUCCESS) {
    Rf_error("Error in mdb: %s: %s", mdb_strerror(x), str);
  }
}

const char * scalar_character(SEXP x, const char * name) {
  if (TYPEOF(x) != STRSXP || length(x) != 1) {
    Rf_error("Expected a scalar character for %s", name);
  }
  return CHAR(STRING_ELT(x, 0));
}

int scalar_int(SEXP x, const char * name) {
  if (TYPEOF(x) != INTSXP || length(x) != 1) {
    Rf_error("Expected a scalar integer for %s", name);
  }
  return INTEGER(x)[0];
}

size_t scalar_size(SEXP x, const char * name) {
  int ret = scalar_int(x, name);
  if (ret < 0) {
    Rf_error("Expected a size for %s", name);
  }
  return (size_t)x;
}

bool scalar_logical(SEXP x, const char * name) {
  if (TYPEOF(x) != LGLSXP || length(x) != 1) {
    Rf_error("Expected a scalar logical for %s", name);
  }
  int ret = INTEGER(x)[0];
  if (ret == NA_LOGICAL) {
    Rf_error("Expected a non-missing scalar logical for %s", name);
  }
  return ret == 1;
}

SEXP r_is_null_pointer(SEXP x) {
  if (TYPEOF(x) != EXTPTRSXP) {
    Rf_error("Expected an external pointer");
  }
  return ScalarLogical(R_ExternalPtrAddr(x) == NULL);
}

SEXP pairlist_create(SEXP x) {
  SEXP ret = R_NilValue;
  size_t n = (size_t) length(x);
  for (size_t i = 0; i < n; ++i) {
    ret = PROTECT(CONS(VECTOR_ELT(x, i), ret));
  }
  UNPROTECT(n);
  return ret;
}

SEXP pairlist_drop(SEXP x, SEXP el) {
  if (x == R_NilValue) {
    return R_NilValue;
  } else {
    if (CAR(x) == el) {
      return CDR(x);
    } else {
      SEXP tail = PROTECT(pairlist_drop(CDR(x), el));
      SEXP ret = CONS(CAR(x), tail);
      UNPROTECT(1);
      return ret;
    }
  }
}
