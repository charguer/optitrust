#include <optitrust.h>

#include "omp.h"

void array_copy(float* A, float* B, int n) {
  __modifies("B ~> Matrix1(n)");
  __reads("A ~> Matrix1(n)");
  for (int i = 0; i < n; ++i) {
    __strict();
    __smodifies("B ~> Matrix1(n)");
    __sreads("A ~> Matrix1(n)");
    __ghost(matrix1_ro_focus, "M := A, i := i");
    __ghost(matrix1_focus, "M := B, i := i");
    B[MINDEX1(n, i)] = A[MINDEX1(n, i)];
    __ghost(matrix1_unfocus, "M := B");
    __ghost(matrix1_ro_unfocus, "M := A");
  }
}

void array_copy_explicit(float* A, float* B, int n) {
  __modifies("B ~> Matrix1(n)");
  __reads("A ~> Matrix1(n)");
  for (int i = 0; i < n; ++i) {
    __strict();
    __smodifies("B ~> Matrix1(n)");
    __sreads("A ~> Matrix1(n)");
    __ghost(matrix1_ro_focus, "M := A, i := i");
    __ghost(matrix1_focus, "M := B, i := i");
    B[MINDEX1(n, i)] = A[MINDEX1(n, i)];
    __ghost(matrix1_unfocus, "M := B");
    __ghost(matrix1_ro_unfocus, "M := A");
  }
}

void array_copy_par(float* A, float* B, int n) {
  __modifies("B ~> Matrix1(n)");
  __reads("A ~> Matrix1(n)");
#pragma omp parallel for
  for (int i = 0; i < n; ++i) {
    __strict();
    __xmodifies("&B[MINDEX1(n, i)] ~> Cell");
    __xreads("&A[MINDEX1(n, i)] ~> Cell");
    B[MINDEX1(n, i)] = A[MINDEX1(n, i)];
  }
}

void array_copy_with_tmp(float* A, float* B, int n) {
  __modifies("B ~> Matrix1(n)");
  __reads("A ~> Matrix1(n)");
  float* const T = (float*)calloc(MSIZE1(n), sizeof(float));
  for (int i = 0; i < n; ++i) {
    __strict();
    __sreads("A ~> Matrix1(n)");
    __xmodifies("&B[MINDEX1(n, i)] ~> Cell");
    __xmodifies("&T[MINDEX1(n, i)] ~> Cell");
    __ghost(matrix1_ro_focus, "M := A, i := i");
    T[MINDEX1(n, i)] = A[MINDEX1(n, i)];
    __ghost(matrix1_ro_unfocus, "M := A");
    B[MINDEX1(n, i)] = T[MINDEX1(n, i)];
  }
  free(T);
}

void g(int* x) { __reads("x ~> Cell"); }

void f(int* x, int* y) {
  __modifies("x ~> Cell");
  __modifies("y ~> Cell");
  *x = 4;
  g(x);
  *y += 1;
  const int a = *x;
}
