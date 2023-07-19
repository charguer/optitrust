#include "../../include/optitrust.h"

__ghost ghost_array_focus(float* M, int i) {
  __requires("dim: int;");
  __consumes("M ~> Array(dim);");
  __produces("M ~> FocussedArray(dim, i); M[i] ~> Cell;");
  __admitted();
}

__ghost ghost_array_unfocus(float* M) {
  __requires("i: int; dim: int;");
  __consumes("M[i] ~> Cell; M ~> FocussedArray(dim, i);");
  __produces("M ~> Array(dim);");
  __admitted();
}

__ghost ghost_array_ro_focus(float* M, int i) {
  __requires("f: _Fraction; dim: int;");
  __consumes("RO(f, M ~> Array(dim));");
  __produces("RO(f, M ~> FocussedArray(dim, i)); RO(f, M[i] ~> Cell);");
  __admitted();
}

__ghost ghost_array_ro_unfocus(float* M) {
  __requires("f: _Fraction; i: int; dim: int;");
  __consumes(
      "RO(_Full(f), M[i] ~> Cell); RO(_Full(f), M ~> FocussedArray(dim, i));");
  __produces("RO(f, M ~> Array(dim));");
  __admitted();
}

void array_copy(float* A, float* B, int n) {
  __modifies("B ~> Array(n);");
  __reads("A ~> Array(n);");
  for (int i = 0; i < n; ++i) {
    ghost_array_ro_focus(A, i);
    ghost_array_focus(B, i);
    B[i] = A[i];
    ghost_array_unfocus(B);
    ghost_array_ro_unfocus(A);
  }
}

void array_copy_explicit(float* A, float* B, int n) {
  __modifies("B ~> Array(n);");
  __reads("A ~> Array(n);");
  for (int i = 0; i < n; ++i) {
    __sequentially_modifies("B ~> Array(n);");
    __sequentially_reads("A ~> Array(n);");
    ghost_array_ro_focus(A, i);
    ghost_array_focus(B, i);
    B[i] = A[i];
    ghost_array_unfocus(B);
    ghost_array_ro_unfocus(A);
  }
}

__ghost ghost_array_unfold(float* M) {
  __requires("dim: int;");
  __consumes("M ~> Array(dim);");
  __produces(
      "Group(range(0, dim, 1), [&] ( auto i )   _HasModel(M[i], Cell));");
  __admitted();
}

__ghost ghost_array_fold(float* M) {
  __requires("dim: int;");
  __consumes(
      "Group(range(0, dim, 1), [&] ( auto i )   _HasModel(M[i], Cell));");
  __produces("M ~> Array(dim);");
  __admitted();
}

void array_copy_par(float* A, float* B, int n) {
  __modifies("B ~> Array(n);");
  __reads("A ~> Array(n);");
  ghost_array_unfold(B);
#pragma omp parallel
  for (int i = 0; i < n; ++i) {
    __sequentially_reads("A ~> Array(n);");
    __modifies("B[i] ~> Cell;");
    ghost_array_ro_focus(A, i);
    B[i] = A[i];
    ghost_array_ro_unfocus(A);
  }
  ghost_array_fold(B);
}

float* array_alloc(int sz) {
  __produces("_Res ~> Array(sz);");
  __admitted();
  return (float*)malloc(sz * sizeof(float));
}

void array_free(float* A) {
  __requires("sz: int;");
  __consumes("A ~> Array(sz);");
  __admitted();
  free(A);
}

void array_copy_with_tmp(float* A, float* B, int n) {
  __modifies("B ~> Array(n);");
  __reads("A ~> Array(n);");
  float* const T = array_alloc(n);
  ghost_array_unfold(T);
  ghost_array_unfold(B);
  for (int i = 0; i < n; ++i) {
    __sequentially_reads("A ~> Array(n);");
    __modifies("T[i] ~> Cell; B[i] ~> Cell;");
    ghost_array_ro_focus(A, i);
    T[i] = A[i];
    ghost_array_ro_unfocus(A);
    B[i] = T[i];
  }
  ghost_array_fold(B);
  ghost_array_fold(T);
  array_free(T);
}

void g(int* x) { __reads("x ~> Cell;"); }

void f(int* x, int* y) {
  __modifies("y ~> Cell; x ~> Cell;");
  *x = 4;
  g(x);
  *y += 1;
  const int a = *x;
}
