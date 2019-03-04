#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>
#include <lmdb.h>

void no_error(int rc, const char* str);
bool no_error2(int rc, int false_flag, const char* str);

const char * scalar_character(SEXP x, const char * name);
int scalar_int(SEXP x, const char * name);
size_t scalar_size(SEXP x, const char * name);
mdb_size_t scalar_mdb_size(SEXP x, const char * name);
bool scalar_logical(SEXP x, const char * name);

SEXP r_is_null_pointer(SEXP x);

// The order here is relied on elsewhere...
typedef enum return_as {
  AS_STRING,
  AS_RAW,
  AS_ANY,
  // Only visible from C:
  AS_CHAR,
} return_as;

return_as to_return_as(SEXP x);

bool is_raw_string(const char* str, size_t len, return_as as_raw);
SEXP raw_string_to_sexp(const char *str, size_t len, return_as as_raw);
size_t sexp_get_data(SEXP data, const char **data_contents, const char* name);
SEXP r_test_error(SEXP r_rc, SEXP r_false_flag, SEXP r_str);

SEXP combine_vector(SEXP x, size_t len);

SEXP r_pointer_addr_str(SEXP r_ptr);
