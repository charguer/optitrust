#include <optitrust_models.h>

double* matrix_alloc_zeroes(int N1, int N2) {
  __produces("_Res ~> Matrix2(N1, N2, fun (i j: int) -> 0.0)");
  __produces("Free(_Res, _Res ~> UninitMatrix2(N1, N2))");
  __admitted();
  double* p = CALLOC2(double, N1, N2);
  return p;
}

void matrix_copy(double* src, double* dst, int n, int m) {
  __requires("M: int * int -> double");
  __reads("src ~> Matrix2(n, m, M)");
  __writes("dst ~> Matrix2(n, m, M)");
  __admitted();
  memcpy(dst, src, n * m * sizeof(double));
}

void test() {
  __pure();
  const int n = 3;

  double* const a = matrix_alloc_zeroes(n, n);
  double* const b = matrix_alloc_zeroes(n, n);
  double* const c = MALLOC2(double, n, n);
  matrix_copy(a, b, n, n);
  matrix_copy(b, c, n, n);

  free(a);
  free(b);
  free(c);
}
