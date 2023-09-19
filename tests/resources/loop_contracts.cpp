#include "../../include/optitrust.h"

__GHOST(array_focus) {
    __requires("M: ptr, i: int, dim: int");
    __consumes("M ~> Array(dim)");
    __produces("&M[i] ~> Cell, M ~> FocussedArray(dim, i)");
    __admitted();
}

__GHOST(array_unfocus) {
    __requires("M: ptr, i: int, dim: int");
    __consumes("M ~> FocussedArray(dim, i), &M[i] ~> Cell");
    __produces("M ~> Array(dim)");
    __admitted();
}

__GHOST(array_ro_focus) {
    __requires("M: ptr, i: int, dim: int, f: _Fraction");
    __consumes("_RO(f, M ~> Array(dim))");
    __produces("_RO(f, &M[i] ~> Cell), _RO(f, M ~> FocussedArray(dim, i))");
    __admitted();
}

__GHOST(array_ro_unfocus) {
    __requires("M: ptr, i: int, dim: int, f: _Fraction");
    __consumes("_RO(_Full(f), M ~> FocussedArray(dim, i)), _RO(_Full(f), &M[i] ~> Cell)");
    __produces("_RO(f, M ~> Array(dim))");
    __admitted();
}

void array_copy(float* A, float* B, int n) {
    __reads("A ~> Array(n)");
    __modifies("B ~> Array(n)");
    for (int i = 0; i < n; ++i) {
        __ghost(array_ro_focus, "A, i"); // Will be automatically inferred later
        __ghost(array_focus, "B, i"); // idem
        B[i] = A[i];
        __ghost(array_unfocus, "B"); // idem
        __ghost(array_ro_unfocus, "A"); // idem
    }
}

void array_copy_explicit(float* A, float* B, int n) {
    __reads("A ~> Array(n)");
    __modifies("B ~> Array(n)");
    for (int i = 0; i < n; ++i) {
        __sequentially_reads("A ~> Array(n)");
        __sequentially_modifies("B ~> Array(n)");

        __ghost(array_ro_focus, "A, i");
        __ghost(array_focus, "B, i");
        B[i] = A[i];
        __ghost(array_unfocus, "B");
        __ghost(array_ro_unfocus, "A");
    }
}

/* for (int i = 0; i < n; ++i) {
 *    __sequentially_reads();
 *    __sequentially_modifies();
 *    __reads();
 *    __modifies(); <- must be empty for parallelizability
 */

__GHOST(array_unfold) {
    __requires("M: ptr, dim: int");
    __consumes("M ~> Array(dim)");
    __produces("Group(range(0, dim, 1), fun i -> &M[i] ~> Cell)");
    __admitted();
}

__GHOST(array_fold) {
    __requires("M: ptr, dim: int");
    __consumes("Group(range(0, dim, 1), fun i -> &M[i] ~> Cell)");
    __produces("M ~> Array(dim)");
    __admitted();
}

void array_copy_par(float* A, float* B, int n) {
    __reads("A ~> Array(n)");
    __modifies("B ~> Array(n)");

    __ghost(array_unfold, "B");
    for (int i = 0; i < n; ++i) {
        __sequentially_reads("A ~> Array(n)");
        __modifies("&B[i] ~> Cell");

        __ghost(array_ro_focus, "A, i"); // Will be removed
        B[i] = A[i];
        __ghost(array_ro_unfocus, "A"); // Will be removed
    }
    __ghost(array_fold, "B");
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
    __reads("A ~> Array(n)");
    __modifies("B ~> Array(n)");

    float* const T = array_alloc(n);
    __ghost(array_unfold, "T");
    __ghost(array_unfold, "B");
    for (int i = 0; i < n; ++i) {
        __sequentially_reads("A ~> Array(n)");
        __modifies("&B[i] ~> Cell, &T[i] ~> Cell");

        __ghost(array_ro_focus, "A, i"); // Will be removed
        T[i] = A[i];
        __ghost(array_ro_unfocus, "A"); // Will be removed
        B[i] = T[i];
    }
    __ghost(array_fold, "B");
    __ghost(array_fold, "T");
    array_free(T);
}

// Loop.fission -> On reprend les contrats et on minimize

void g(int* x) {
    __reads("x ~> Cell");
}

void f(int* x, int* y) {
    __modifies("x ~> Cell, y ~> Cell"); // Gives separation
    *x = 4;
    g(x);
    *y += 1;
    const int a = *x;
}

/*// Last priority
void line_sum(float* A, float* S, int n) {
    __reads("A ~> Matrix2(n, n);");
    __modifies("S ~> Array(n);");
    for (int i = 0; i < n; ++i) {
        S[i] = 0;
        for (int j = 0; j < n; ++j) {
            S[i] += A[MINDEX(i, j)];
        }
    }
}*/
