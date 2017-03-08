#include <R_ext/Rdynload.h>
#include <R.h>
#include "taxaTrim.h"

void R_init_taxonomizr(DllInfo* info) {
  R_registerRoutines(info, NULL, NULL, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}
