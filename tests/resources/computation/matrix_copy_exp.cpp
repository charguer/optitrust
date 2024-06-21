#include <optitrust.h>

float* matrix_alloc(int N1, int N2) {
  __produces("_Res ~> Matrix2(N1, N2)");
  __admitted();
  float* p = (float*)malloc((long unsigned int)(N1 * N2) * sizeof(float));
  return p;
}

void matrix_free(float* p) {
  __requires("N1: int");
  __requires("N2: int");
  __consumes("p ~> Matrix2(N1, N2)");
  __admitted();
  free(p);
}

void matrix_copy(float* src, float* dst, int n, int m) {
  __modifies("dst ~> Matrix2(n, m)");
  __reads("src ~> Matrix2(n, m)");
  __admitted();
  memcpy(dst, src, (long unsigned int)(n * m) * sizeof(float));
}

int main() {
  __pure();
  const int n = 3;
  float* const a = matrix_alloc(n, n);
  float* const b = matrix_alloc(n, n);
  float* const c = matrix_alloc(n, n);
  matrix_copy(a, b, n, n);
  matrix_copy(b, c, n, n);
  matrix_free(a);
  matrix_free(b);
  matrix_free(c);
}
