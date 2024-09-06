#include "util.h"

void no_error(int rc, const char* str) {
  if (rc != MDB_SUCCESS) {
    Rf_error("Error in mdb: %s: %s (code: %d)", mdb_strerror(rc), str, rc);
  }
}

bool no_error2(int rc, int false_flag, const char* str) {
  bool found = false;
  if (rc == MDB_SUCCESS) {
    found = true;
  } else if (rc != false_flag) {
    Rf_error("Error in mdb: %s: %s (code: %d)", mdb_strerror(rc), str, rc);
  }
  return found;
}

const char * scalar_character(SEXP x, const char * name) {
  if (TYPEOF(x) != STRSXP || Rf_length(x) != 1) {
    Rf_error("Expected a scalar character for '%s'", name);
  }
  return CHAR(STRING_ELT(x, 0));
}

int scalar_int(SEXP x, const char * name) {
  if (TYPEOF(x) != INTSXP || Rf_length(x) != 1) {
    Rf_error("Expected a scalar integer for '%s'", name);
  }
  return INTEGER(x)[0];
}


mdb_size_t scalar_mdb_size(SEXP x, const char * name) {
  mdb_size_t ret = 0;
  if (TYPEOF(x) == INTSXP && Rf_length(x) == 1) {
    int value = INTEGER(x)[0];
    if (value < 0) {
      Rf_error("Expected a positive size for '%s'", name);
    }
    ret = value;
  } else if (TYPEOF(x) == REALSXP && Rf_length(x) == 1) {
    double value = REAL(x)[0];
    if (value < 0) {
      Rf_error("Expected a positive size for '%s'", name);
    }
    ret = value;
  } else {
    // NOTE: not typically reachable from R code (see test-env.R)
    Rf_error("Expected a scalar integer for '%s'", name);
  }
  return ret;
}


size_t scalar_size(SEXP x, const char * name) {
  int ret = scalar_int(x, name);
  if (ret < 0) {
    Rf_error("Expected a positive size for '%s'", name);
  }
  return (size_t)ret;
}

bool scalar_logical(SEXP x, const char * name) {
  if (TYPEOF(x) != LGLSXP || Rf_length(x) != 1) {
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
  return Rf_ScalarLogical(R_ExternalPtrAddr(x) == NULL);
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
    if (has_raw && as_raw != AS_ANY) {
      Rf_error("value contains embedded nul bytes; cannot return string");
    }
    return has_raw;
  }
}

SEXP raw_string_to_sexp(const char *str, size_t len, return_as as_raw) {
  bool is_raw = is_raw_string(str, len, as_raw);
  SEXP ret;
  if (is_raw) {
    ret = PROTECT(Rf_allocVector(RAWSXP, len));
    memcpy(RAW(ret), str, len);
  } else if (as_raw == AS_CHAR) {
    ret = PROTECT(Rf_mkCharLen(str, len));
  } else {
    ret = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(ret, 0, Rf_mkCharLen(str, len));
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
    if (Rf_length(data) != 1) {
      Rf_error("'%s' must be a scalar character", name);
    }
    SEXP el = STRING_ELT(data, 0);
    *data_contents = CHAR(el);
    return Rf_length(el);
    break;
  case RAWSXP:
    *data_contents = (const char*) RAW(data);
    return Rf_length(data);
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
  return Rf_ScalarLogical(ret);
}

SEXP combine_vector(SEXP x, size_t len) {
  if (len == (size_t)Rf_length(x)) {
    return x;
  }
  bool out_str = TYPEOF(x) == STRSXP;
  SEXP ret = PROTECT(Rf_allocVector(TYPEOF(x), len));
  size_t n = Rf_length(x);
  for (size_t i = 0, j = 0; i < len; ++i, ++j) {
    if (j == n) {
      x = Rf_getAttrib(x, Rf_install("next"));
      n = Rf_length(x);
      j = 0;
    }
    if (out_str) {
      SET_STRING_ELT(ret, i, STRING_ELT(x, j));
    } else {
      SET_VECTOR_ELT(ret, i, VECTOR_ELT(x, j));
    }
  }
  UNPROTECT(1);
  return ret;
}

SEXP r_pointer_addr_str(SEXP r_ptr) {
  if (TYPEOF(r_ptr) != EXTPTRSXP) {
    Rf_error("Expected an external pointer");
  }
  void * ptr = R_ExternalPtrAddr(r_ptr);
  char buf[128];
  snprintf(buf, 128, "%p", ptr);
  return Rf_mkString(buf);
}
