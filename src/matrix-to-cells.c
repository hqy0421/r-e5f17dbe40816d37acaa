#include "tibble.h"

// One integer vector of 1-based row positions of the TRUE cells per column,
// in column-major order (column by column, rows in original order).
// NA cells are skipped, matching which().
// All allocations are proportional to the number of columns and TRUE cells,
// never a copy of or an index over the whole mask.
SEXP tibble_matrix_to_cells(SEXP j) {
  if (TYPEOF(j) != LGLSXP) {
    Rf_error("Internal error: `j` must be a logical matrix.");
  }
  SEXP dim = Rf_getAttrib(j, R_DimSymbol);
  if (TYPEOF(dim) != INTSXP || Rf_length(dim) != 2) {
    Rf_error("Internal error: `j` must be a logical matrix.");
  }

  const R_xlen_t nr = INTEGER(dim)[0];
  const R_xlen_t nc = INTEGER(dim)[1];
  const int* vj = LOGICAL(j);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, nc));

  for (R_xlen_t col = 0; col < nc; col++) {
    const int* vcol = vj + col * nr;

    int count = 0;
    for (R_xlen_t row = 0; row < nr; row++) {
      if (vcol[row] == TRUE) {
        count++;
      }
    }

    SEXP cells = Rf_allocVector(INTSXP, count);
    SET_VECTOR_ELT(out, col, cells);

    int* vcells = INTEGER(cells);
    int k = 0;
    for (R_xlen_t row = 0; row < nr; row++) {
      if (vcol[row] == TRUE) {
        vcells[k++] = (int)(row + 1);
      }
    }
  }

  UNPROTECT(1);
  return out;
}
