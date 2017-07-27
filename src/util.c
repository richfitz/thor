#include "util.h"

void no_error(int rc, const char* str) {
  if (rc != MDB_SUCCESS) {
    Rf_error("Error in mdb: %s: %s", mdb_strerror(rc), str);
  }
}

bool no_error2(int rc, int false_flag, const char* str) {
  bool found = false;
  if (rc == MDB_SUCCESS) {
    found = true;
  } else if (rc != false_flag) {
    Rf_error("Error in mdb: %s: %s", mdb_strerror(rc), str);
  }
  return found;
}

const char * scalar_character(SEXP x, const char * name) {
  if (TYPEOF(x) != STRSXP || length(x) != 1) {
    Rf_error("Expected a scalar character for '%s'", name);
  }
  return CHAR(STRING_ELT(x, 0));
}

int scalar_int(SEXP x, const char * name) {
  if (TYPEOF(x) != INTSXP || length(x) != 1) {
    Rf_error("Expected a scalar integer for '%s'", name);
  }
  return INTEGER(x)[0];
}

size_t scalar_size(SEXP x, const char * name) {
  int ret = scalar_int(x, name);
  if (ret < 0) {
    Rf_error("Expected a positive size for '%s'", name);
  }
  return (size_t)ret;
}

bool scalar_logical(SEXP x, const char * name) {
  if (TYPEOF(x) != LGLSXP || length(x) != 1) {
    Rf_error("Expected a scalar logical for '%s'", name);
  }
  int ret = INTEGER(x)[0];
  if (ret == NA_LOGICAL) {
    Rf_error("Expected a non-missing scalar logical for '%s'", name);
  }
  return ret == 1;
}

SEXP r_is_null_pointer(SEXP x) {
  if (TYPEOF(x) != EXTPTRSXP) {
    Rf_error("Expected an external pointer");
  }
  return ScalarLogical(R_ExternalPtrAddr(x) == NULL);
}

return_as to_return_as(SEXP x) {
  if (x == R_NilValue) {
    return AS_ANY;
  } else if (TYPEOF(x) == LGLSXP && LENGTH(x) == 1) {
    int as_raw = INTEGER(x)[0];
    if (as_raw == NA_LOGICAL) {
      Rf_error("Expected a non-missing logical scalar (or NULL) for 'as_raw'");
    }
    return as_raw ? AS_RAW : AS_STRING;
  } else {
    Rf_error("Expected a logical scalar (or NULL) for 'as_raw'");
    return AS_ANY;
  }
}

bool is_raw_string(const char* str, size_t len, return_as as_raw) {
  if (as_raw == AS_RAW) {
    return true;
  } else {
    bool has_raw = memchr(str, '\0', len) != NULL;
    if (has_raw && as_raw == AS_STRING) {
      Rf_error("value contains embedded nul bytes; cannot return string");
    }
    return has_raw;
  }
}

// This is the same strategy as redux.
SEXP raw_string_to_sexp(const char *str, size_t len, return_as as_raw) {
  bool is_raw = is_raw_string(str, len, as_raw);
  SEXP ret;
  if (is_raw) {
    ret = PROTECT(allocVector(RAWSXP, len));
    memcpy(RAW(ret), str, len);
  } else {
    ret = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(ret, 0, mkCharLen(str, len));
  }
  UNPROTECT(1);
  return ret;
}

size_t sexp_get_data(SEXP data, const char **data_contents, const char* name) {
  switch (TYPEOF(data)) {
    // Evenually this might be useful:
    // case CHARSXP:
    // *data_contents = CHAR(data);
    // return length(data);
    // break;
  case STRSXP:
    if (length(data) != 1) {
      Rf_error("'%s' must be a scalar character", name);
    }
    SEXP el = STRING_ELT(data, 0);
    *data_contents = CHAR(el);
    return length(el);
    break;
  case RAWSXP:
    *data_contents = (const char*) RAW(data);
    return length(data);
  default:
    Rf_error("Invalid data type for '%s'; expected string or raw", name);
  }
}

SEXP r_test_error(SEXP r_rc, SEXP r_false_flag, SEXP r_str) {
  int rc = scalar_int(r_rc, "rc");
  const char * str = scalar_character(r_str, "str");
  bool ret = true;
  if (r_false_flag == R_NilValue) {
    no_error(rc, str);
  } else {
    int false_flag = scalar_int(r_false_flag, "false_flag");
    ret = no_error2(rc, false_flag, str);
  }
  return ScalarLogical(ret);
}
