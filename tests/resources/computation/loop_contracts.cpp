#include <optitrust.h>

void array_copy(float* A, float* B, int n) {
  __reads("A ~> Matrix1(n)");
  __modifies("B ~> Matrix1(n)");
  for (int i = 0; i < n; ++i) {
      __ghost(ro_matrix1_focus, "A, i"); // Will be automatically inferred later
      __ghost(matrix1_focus, "B, i"); // idem
      B[MINDEX1(n,i)] = A[MINDEX1(n,i)];
      __ghost(matrix1_unfocus, "B"); // idem
      __ghost(ro_matrix1_unfocus, "A"); // idem
  }
}

void array_copy_explicit(float* A, float* B, int n) {
  __reads("A ~> Matrix1(n)");
  __modifies("B ~> Matrix1(n)");
  for (int i = 0; i < n; ++i) {
      __strict();
      __sreads("A ~> Matrix1(n)");
      __smodifies("B ~> Matrix1(n)");

      __ghost(ro_matrix1_focus, "A, i");
      __ghost(matrix1_focus, "B, i");
      B[MINDEX1(n,i)] = A[MINDEX1(n,i)];
      __ghost(matrix1_unfocus, "B");
      __ghost(ro_matrix1_unfocus, "A");
  }
}

/* for (int i = 0; i < n; ++i) {
 *    __sreads();
 *    __smodifies();
 *    __xreads();
 *    __xmodifies(); <- must be empty for parallelizability
 */

void array_copy_par(float* A, float* B, int n) {
  __reads("A ~> Matrix1(n)");
  __modifies("B ~> Matrix1(n)");

  for (int i = 0; i < n; ++i) {
    __strict();
    __xreads("&A[MINDEX1(n,i)] ~> Cell");
    __xmodifies("&B[MINDEX1(n,i)] ~> Cell");
    B[MINDEX1(n,i)] = A[MINDEX1(n,i)];
  }
}

void array_copy_with_tmp(float* A, float* B, int n) {
  __reads("A ~> Matrix1(n)");
  __modifies("B ~> Matrix1(n)");

  float* const T = CALLOC1(float, n);
  for (int i = 0; i < n; ++i) {
    __strict();
    __sreads("A ~> Matrix1(n)");
    __xmodifies("&B[MINDEX1(n,i)] ~> Cell, &T[MINDEX1(n,i)] ~> Cell");

    __ghost(ro_matrix1_focus, "A, i"); // Will be removed
    T[MINDEX1(n,i)] = A[MINDEX1(n,i)];
    __ghost(ro_matrix1_unfocus, "A"); // Will be removed
    B[MINDEX1(n,i)] = T[MINDEX1(n,i)];
  }
  free(T);
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
