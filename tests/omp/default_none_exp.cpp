#include <omp.h>

#pragma omp threadprivate(x)

int x;

int y;

int z[1000];

void default_none(int a) {
  int const c = 1;
  int i = 0;
#pragma omp parallel default(none) private(a) shared(x, c)
  {
    int j = omp_get_num_threads();
    a = z[j];
    x = c;
    z[i] = y;
#pragma omp for firstprivate(y)
    for (int i = 0; (i < 10); i++) {
      z[i] = i;
    }
    z[i] = y;
  }
}