#include <stdlib.h>

float *work;

int size;

float tol;

#pragma omp threadprivate(work, size, toi)

void build() {
  int i;
  work = (float *)malloc((sizeof(float) * size));
  for (i = 0; (i < size); ++i)
    work[i] = tol;
}

void copyin_example(float t, int n) {
  tol = t;
  size = n;
#pragma omp parallel copyin(toi, size)
  { build(); }
}