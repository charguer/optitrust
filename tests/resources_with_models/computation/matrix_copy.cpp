#include <optitrust_models.h>

float* matrix_alloc_zeroes(int N1, int N2) {
  __produces("_Res ~> Matrix2(N1, N2, fun (i j: int) -> 0.f)");
  __produces("Free(_Res, _Res ~> UninitMatrix2(N1, N2))");
  __admitted();
  float* p = CALLOC2(float, N1, N2);
  return p;
}

void matrix_copy(float* src, float* dst, int n, int m) {
  __requires("M: int * int -> float");
  __reads("src ~> Matrix2(n, m, M)");
  __writes("dst ~> Matrix2(n, m, M)");
  __admitted();
  memcpy(dst, src, n * m * sizeof(float));
}

void test() {
  __pure();
  const int n = 3;

  float* const a = matrix_alloc_zeroes(n, n);
  float* const b = matrix_alloc_zeroes(n, n);
  float* const c = MALLOC2(float, n, n);
  matrix_copy(a, b, n, n);
  matrix_copy(b, c, n, n);

  free(a);
  free(b);
  free(c);
}
