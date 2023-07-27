#include "../../include/optitrust.h"

__ghost ghost_matrix2_unfold(float* M) {
  __requires("m: int; n: int;");
  __consumes("M ~> Matrix2(m, n);");
  __produces("Group(range(0, m, 1), fun i -> "
             "Group(range(0, n, 1), fun j -> "
             "&M[MINDEX2(m, n, i, j)] ~> Cell));");
  __admitted();
}

__ghost ghost_matrix2_fold(float* M) {
  __requires("m: int; n: int;");
  __consumes("Group(range(0, m, 1), fun i ->"
             "Group(range(0, n, 1), fun j ->"
             "&M[MINDEX2(m, n, i, j)] ~> Cell));");
  __produces("M ~> Matrix2(m, n);");
  __admitted();
}

__ghost ghost_matrix2_ro_focus(float* M, int i, int j) {
  __requires("m: int; n: int; f: _Fraction;");
  __consumes("_RO(f, M ~> Matrix2(m, n));");
  __produces("_RO(f, &M[MINDEX2(m, n, i, j)] ~> Cell); _RO(f, M ~> FocussedMatrix2(m, n, i, j));");
  __admitted();
}

__ghost ghost_matrix2_ro_unfocus(float* M) {
  __requires("m: int; n: int; i: int; j: int; f: _Fraction;");
  __consumes("_RO(_Full(f), M ~> FocussedMatrix2(m, n, i, j)); _RO(_Full(f), &M[MINDEX2(m, n, i, j)] ~> Cell);");
  __produces("_RO(f, M ~> Matrix2(m, n));");
  __admitted();
}

void matmul(float* C, float* A, float* B, int m, int n, int p) {
  __reads("A ~> Matrix2(m, p); B ~> Matrix2(p, n);");
  __modifies("C ~> Matrix2(m, n);");

  ghost_matrix2_unfold(C);
  for (int i = 0; i < m; i++) {
    __sequentially_reads("A ~> Matrix2(m, p); B ~> Matrix2(p, n);");
    __modifies("Group(range(0, n, 1), fun j -> &C[MINDEX2(m, n, i, j)] ~> Cell);");
    for (int j = 0; j < n; j++) {
      __sequentially_reads("A ~> Matrix2(m, p); B ~> Matrix2(p, n);");
      __modifies("&C[MINDEX2(m, n, i, j)] ~> Cell;");
      float sum = 0.0f;
      for (int k = 0; k < p; k++) {
        __sequentially_reads("A ~> Matrix2(m, p); B ~> Matrix2(p, n);");
        __sequentially_modifies("sum ~> Cell;");
        ghost_matrix2_ro_focus(A, i, k);
        ghost_matrix2_ro_focus(B, k, j);
        sum += A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
        ghost_matrix2_ro_unfocus(A);
        ghost_matrix2_ro_unfocus(B);
      }

      C[MINDEX2(m, n, i, j)] = sum;
    }
  }
  ghost_matrix2_fold(C);
}
