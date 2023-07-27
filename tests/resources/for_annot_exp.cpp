#include "../../include/optitrust.h"

__ghost ghost_matrix2_focus(float* M, int i, int j) {
  __requires("n: int; m: int;");
  __consumes("M ~> Matrix2(m, n);");
  __produces("M ~> FocussedMatrix2(m, n, i); M[MINDEX2(m, n, i, j)] ~> Cell;");
  __admitted();
}

__ghost ghost_matrix2_unfocus(float* M) {
  __requires("j: int; i: int; n: int; m: int;");
  __consumes("M[MINDEX2(m, n, i, j)] ~> Cell; M ~> FocussedMatrix2(m, n, i);");
  __produces("M ~> Matrix2(m, n);");
  __admitted();
}

__ghost ghost_matrix2_ro_focus(float* M, int i, int j) {
  __requires("f: _Fraction; n: int; m: int;");
  __consumes("RO(f, M ~> Matrix2(m, n));");
  __produces(
      "RO(f, M ~> FocussedMatrix2(m, n, i, j)); RO(f, M[MINDEX2(m, n, i, j)] "
      "~> Cell);");
  __admitted();
}

__ghost ghost_matrix2_ro_unfocus(float* M) {
  __requires("f: _Fraction; j: int; i: int; n: int; m: int;");
  __consumes(
      "RO(_Full(f), M[MINDEX2(m, n, i, j)] ~> Cell); RO(_Full(f), M ~> "
      "FocussedMatrix2(m, n, i, j));");
  __produces("RO(f, M ~> Matrix2(m, n));");
  __admitted();
}

void matmul(float* C, float* A, float* B, int m, int n, int p) {
  __modifies("C ~> Matrix2(m, n);");
  __reads("B ~> Matrix2(p, n); A ~> Matrix2(m, p);");
  for (int i = 0; i < m; i++) {
    __sequentially_modifies("C ~> Matrix2(m, n);");
    __sequentially_reads("B ~> Matrix2(p, n); A ~> Matrix2(m, p);");
    for (int j = 0; j < n; j++) {
      float sum = 0.f;
      for (int k = 0; k < p; k++) {
        __sequentially_modifies("sum ~> Cell;");
        __sequentially_reads("B ~> Matrix2(p, n); A ~> Matrix2(m, p);");
        ghost_matrix2_ro_focus(A, i, k);
        ghost_matrix2_ro_focus(B, k, j);
        sum += A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
        ghost_matrix2_ro_unfocus(A);
        ghost_matrix2_ro_unfocus(B);
      }
      ghost_matrix2_focus(C, i, j);
      C[MINDEX2(m, n, i, j)] = sum;
      ghost_matrix2_unfocus(C);
    }
  }
}
