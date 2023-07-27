#include "../../include/optitrust.h"

__ghost ghost_matrix2_unfold(float* M) {
  __requires("n: int; m: int;");
  __consumes("M ~> Matrix2(m, n);");
  __produces(
      "Group(range(0, m, 1), [&] ( auto i )   Group(range(0, n, 1), [&] ( auto "
      "j\n)   _HasModel(M[MINDEX2(m, n, i, j)], Cell)));");
  __admitted();
}

__ghost ghost_matrix2_fold(float* M) {
  __requires("n: int; m: int;");
  __consumes(
      "Group(range(0, m, 1), [&] ( auto i )   Group(range(0, n, 1), [&] ( auto "
      "j\n)   _HasModel(M[MINDEX2(m, n, i, j)], Cell)));");
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
  ghost_matrix2_unfold(C);
  for (int i = 0; i < m; i++) {
    __sequentially_reads("B ~> Matrix2(p, n); A ~> Matrix2(m, p);");
    __modifies(
        "Group(range(0, n, 1), [&] ( auto j )   _HasModel(C[MINDEX2(m, n, i, "
        "j)], Cell));");
    for (int j = 0; j < n; j++) {
      __sequentially_reads("B ~> Matrix2(p, n); A ~> Matrix2(m, p);");
      __modifies("C[MINDEX2(m, n, i, j)] ~> Cell;");
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
      C[MINDEX2(m, n, i, j)] = sum;
    }
  }
  ghost_matrix2_fold(C);
}
