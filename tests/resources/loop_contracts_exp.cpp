#include "../../include/optitrust.h"
#include "omp.h"

__ghost_ret array_focus() {
  __requires("M: ptr");
  __requires("i: int");
  __requires("dim: int");
  __consumes("M ~> Array(dim)");
  __produces("&M[i] ~> Cell");
  __produces("M ~> FocussedArray(dim, i)");
  __admitted();
}

__ghost_ret array_unfocus() {
  __requires("M: ptr");
  __requires("i: int");
  __requires("dim: int");
  __consumes("M ~> FocussedArray(dim, i)");
  __consumes("&M[i] ~> Cell");
  __produces("M ~> Array(dim)");
  __admitted();
}

__ghost_ret array_ro_focus() {
  __requires("M: ptr");
  __requires("i: int");
  __requires("dim: int");
  __requires("f: _Fraction");
  __consumes("_RO(f, M ~> Array(dim))");
  __produces("_RO(f, &M[i] ~> Cell)");
  __produces("_RO(f, M ~> FocussedArray(dim, i))");
  __admitted();
}

__ghost_ret array_ro_unfocus() {
  __requires("M: ptr");
  __requires("i: int");
  __requires("dim: int");
  __requires("f: _Fraction");
  __consumes("_RO(_Full(f), M ~> FocussedArray(dim, i))");
  __consumes("_RO(_Full(f), &M[i] ~> Cell)");
  __produces("_RO(f, M ~> Array(dim))");
  __admitted();
}

void array_copy(float* A, float* B, int n) {
  __modifies("B ~> Array(n)");
  __reads("A ~> Array(n)");
  for (int i = 0; i < n; ++i) {
    __ghost(array_ro_focus, "M := A, i := i");
    __ghost(array_focus, "M := B, i := i");
    B[i] = A[i];
    __ghost(array_unfocus, "M := B");
    __ghost(array_ro_unfocus, "M := A");
  }
}

void array_copy_explicit(float* A, float* B, int n) {
  __modifies("B ~> Array(n)");
  __reads("A ~> Array(n)");
  for (int i = 0; i < n; ++i) {
    __sequentially_modifies("B ~> Array(n)");
    __sequentially_reads("A ~> Array(n)");
    __ghost(array_ro_focus, "M := A, i := i");
    __ghost(array_focus, "M := B, i := i");
    B[i] = A[i];
    __ghost(array_unfocus, "M := B");
    __ghost(array_ro_unfocus, "M := A");
  }
}

__ghost_ret array_unfold() {
  __requires("M: ptr");
  __requires("dim: int");
  __consumes("M ~> Array(dim)");
  __produces("Group(range(0, dim, 1), fun i -> &M[i] ~> Cell)");
  __admitted();
}

__ghost_ret array_fold() {
  __requires("M: ptr");
  __requires("dim: int");
  __consumes("Group(range(0, dim, 1), fun i -> &M[i] ~> Cell)");
  __produces("M ~> Array(dim)");
  __admitted();
}

__ghost_ret ro_array_unfold() {
  __requires("M: ptr");
  __requires("dim: int");
  __requires("f: _Fraction");
  __consumes("_RO(f, M ~> Array(dim))");
  __produces("_RO(f, Group(range(0, dim, 1), fun i -> &M[i] ~> Cell))");
  __admitted();
}

__ghost_ret ro_array_fold() {
  __requires("M: ptr");
  __requires("dim: int");
  __requires("f: _Fraction");
  __consumes("_RO(_Full(f), Group(range(0, dim, 1), fun i -> &M[i] ~> Cell))");
  __produces("_RO(f, M ~> Array(dim))");
  __admitted();
}

void array_copy_par(float* A, float* B, int n) {
  __modifies("B ~> Array(n)");
  __reads("A ~> Array(n)");
  __ghost(ro_array_unfold, "M := A");
  __ghost(array_unfold, "M := B");
#pragma omp parallel for
  for (int i = 0; i < n; ++i) {
    __modifies("&B[i] ~> Cell");
    __reads("&A[i] ~> Cell");
    B[i] = A[i];
  }
  __ghost(array_fold, "M := B");
  __ghost(ro_array_fold, "M := A");
}

float* array_alloc(int sz) {
  __produces("_Res ~> Array(sz)");
  __admitted();
  return (float*)malloc(sz * sizeof(float));
}

void array_free(float* A) {
  __requires("sz: int");
  __consumes("A ~> Array(sz)");
  __admitted();
  free(A);
}

void array_copy_with_tmp(float* A, float* B, int n) {
  __modifies("B ~> Array(n)");
  __reads("A ~> Array(n)");
  float* const T = array_alloc(n);
  __ghost(array_unfold, "M := T");
  __ghost(array_unfold, "M := B");
  for (int i = 0; i < n; ++i) {
    __sequentially_reads("A ~> Array(n)");
    __modifies("&B[i] ~> Cell");
    __modifies("&T[i] ~> Cell");
    __ghost(array_ro_focus, "M := A, i := i");
    T[i] = A[i];
    __ghost(array_ro_unfocus, "M := A");
    B[i] = T[i];
  }
  __ghost(array_fold, "M := B");
  __ghost(array_fold, "M := T");
  array_free(T);
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
