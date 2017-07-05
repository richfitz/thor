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
