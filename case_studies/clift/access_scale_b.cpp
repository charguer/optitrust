#include <optitrust.h>

#include <cmath>

const int GS = 32;

void accesses(float* v, int n) {
  __reads("v ~> Matrix1(n)");
  float a = 0.f;
  const float sum = 1.f / get_max(v, n);
  float* const w = (float*)malloc(MSIZE1(n) * sizeof(float));
  __ghost([&]() {
    __modifies("w ~> UninitMatrix1(n)");
    __admitted();
    __with("justif := shift_groups");
  });
  for (int i1 = 0; i1 < n; i1++) {
    __xwrites("&w[MINDEX1(n, i1)] ~> Cell");
    __xreads("&v[MINDEX1(n,i1)] ~> Cell");
    w[MINDEX1(n, i1)] = v[MINDEX1(n, i1)];
  }
  for (int i = 0; i < n; i++) {
    __xreads("&w[MINDEX1(n,i)] ~> Cell");
    a = a + w[MINDEX1(n, i)];
  }
  __ghost([&]() {
    __modifies("w ~> UninitMatrix1(n)");
    __admitted();
    __with("justif := shift_groups");
  });
  free(w);
}
