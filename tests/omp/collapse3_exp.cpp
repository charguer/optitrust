#include <omp.h>

#include <stdio.h>

void work(int a, int j, int k);

void sub() {
  int a;
#pragma omp parallel num_threads(2)
  {
#pragma omp for collapse(2) ordered private(j, k) schedule(static, 3)
    for (int k = 1; (k < 3); k++)
      for (int j = 1; (j < 2); j++) {
#pragma omp ordered
        printf("%d %d %d\n", omp_get_thread_num(), k, j);
        work(a, j, k);
      }
  }
}