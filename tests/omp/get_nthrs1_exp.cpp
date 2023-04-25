#include <omp.h>

void work(int i);

void incorrect() {
  int np, i;
  int np = omp_get_num_threads();
#pragma omp parallel for schedule(static)
  for (i = 0; i < &np; i++) {
    work(i);
  }
}
