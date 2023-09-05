#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

void __pure() {}

void __requires(const char*) {}

void __ensures(const char*) {}

void __invariant(const char*) {}

void __reads(const char*) {}

void __modifies(const char*) {}

void __consumes(const char*) {}

void __produces(const char*) {}

void __sequentially_reads(const char*) {}

void __sequentially_modifies(const char*) {}

void __admitted() {}

typedef void __ghost;

int MINDEX0() {
  __pure();
  __admitted();
  return 0;
}

int MINDEX1(int N1, int i1) {
  __pure();
  __admitted();
  return i1;
}

int MINDEX2(int N1, int N2, int i1, int i2) {
  __pure();
  __admitted();
  return i1 * N2 + i2;
}

int MINDEX3(int N1, int N2, int N3, int i1, int i2, int i3) {
  __pure();
  __admitted();
  return i1 * N2 * N3 + i2 * N3 + i3;
}

int MINDEX4(int N1, int N2, int N3, int N4, int i1, int i2, int i3, int i4) {
  __pure();
  __admitted();
  return i1 * N2 * N3 * N4 + i2 * N3 * N4 + i3 * N4 + i4;
}

void* CALLOC1(int N1, size_t bytes_per_item) {
  __produces("_Res ~> Matrix1(N1);");
  __admitted();
  return calloc(N1, bytes_per_item);
}

void* CALLOC2(int N1, int N2, size_t bytes_per_item) {
  __produces("_Res ~> Matrix2(N1, N2);");
  __admitted();
  return calloc(N1 * N2, bytes_per_item);
}

void* CALLOC3(int N1, int N2, int N3, size_t bytes_per_item) {
  __produces("_Res ~> Matrix3(N1, N2, N3);");
  __admitted();
  return calloc(N1 * N2 * N3, bytes_per_item);
}

void* CALLOC4(int N1, int N2, int N3, int N4, size_t bytes_per_item) {
  __produces("_Res ~> Matrix4(N1, N2, N3, N4);");
  __admitted();
  return calloc(N1 * N2 * N3 * N4, bytes_per_item);
}

void* MALLOC0(size_t bytes_per_item) {
  __produces("_Res ~> Matrix0();");
  __admitted();
  return malloc(bytes_per_item);
}

void* MALLOC1(int N1, size_t bytes_per_item) {
  __produces("_Res ~> Matrix1(N1);");
  __admitted();
  return malloc(N1 * bytes_per_item);
}

void* MALLOC2(int N1, int N2, size_t bytes_per_item) {
  __produces("_Res ~> Matrix2(N1, N2);");
  __admitted();
  return malloc(N1 * N2 * bytes_per_item);
}

void* MALLOC3(int N1, int N2, int N3, size_t bytes_per_item) {
  __produces("_Res ~> Matrix3(N1, N2, N3);");
  __admitted();
  return malloc(N1 * N2 * N3 * bytes_per_item);
}

void* MALLOC4(int N1, int N2, int N3, int N4, size_t bytes_per_item) {
  __produces("_Res ~> Matrix4(N1, N2, N3, N4);");
  __admitted();
  return malloc(N1 * N2 * N3 * N4 * bytes_per_item);
}

void MFREE0(void* p) {
  __consumes("p ~> Matrix0();");
  __admitted();
  free(p);
}

void MFREE1(int N1, void* p) {
  __consumes("p ~> Matrix1(N1);");
  __admitted();
  free(p);
}

void MFREE2(int N1, int N2, void* p) {
  __consumes("p ~> Matrix2(N1, N2);");
  __admitted();
  free(p);
}

void MFREE3(int N1, int N2, int N3, void* p) {
  __consumes("p ~> Matrix3(N1, N2, N3);");
  __admitted();
  free(p);
}

void MFREE4(int N1, int N2, int N3, int N4, void* p) {
  __consumes("p ~> Matrix4(N1, N2, N3, N4);");
  __admitted();
  free(p);
}

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
  __consumes("_RO(f, M ~> Matrix2(m, n));");
  __produces(
      "_RO(f, M ~> FocussedMatrix2(m, n, i, j)); _RO(f, M[MINDEX2(m, n, i, j)] "
      "~> Cell);");
  __admitted();
}

__ghost ghost_matrix2_ro_unfocus(float* M) {
  __requires("f: _Fraction; j: int; i: int; n: int; m: int;");
  __consumes(
      "_RO(_Full(f), M[MINDEX2(m, n, i, j)] ~> Cell); _RO(_Full(f), M ~> "
      "FocussedMatrix2(m, n, i, j));");
  __produces("_RO(f, M ~> Matrix2(m, n));");
  __admitted();
}

int exact_div(int n, int b) {
  __pure();
  __admitted();
  return n / b;
}

void MFREE(void* p) { free(p); }

int ANY(int maxValue) { return 0; }

void matmul(float* C, float* A, float* B, int m, int n, int p) {
  __modifies("C ~> Matrix2(m, n);");
  __reads("B ~> Matrix2(p, n); A ~> Matrix2(m, p);");
  for (int i = 0; i < m; i++) {
    __sequentially_reads("B ~> Matrix2(p, n); A ~> Matrix2(m, p);");
    __modifies(
        "Group(range(0, n, 1), fun j -> C[MINDEX2(m, n, i, j)] ~> Cell);");
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
}
