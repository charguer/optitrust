#include <stdio.h>

#include <omp.h>

int main() {
  omp_set_nested(1);
  omp_set_dynamic(0);
#pragma omp parallel
  {
#pragma omp parallel
    {
#pragma omp single
      { printf("Inner: num_thds=%d\n", omp_get_num_threads()); }
    }
#pragma omp parallel
    {
#pragma omp single
      { printf("Inner: num_thds=%d\n", omp_get_num_threads()); }
    }
#pragma omp barrier
#pragma omp single
    { printf("Outer: num_thds=%d\n", omp_get_num_threads()); }
    return 0;
  }
}