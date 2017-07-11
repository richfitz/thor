#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>
#include "lmdb.h"

void no_error(int x, const char* str);

const char * scalar_character(SEXP x, const char * name);
int scalar_int(SEXP x, const char * name);
size_t scalar_size(SEXP x, const char * name);
bool scalar_logical(SEXP x, const char * name);

SEXP r_is_null_pointer(SEXP x);

SEXP pairlist_create(SEXP x);
SEXP pairlist_drop(SEXP x, SEXP el);
