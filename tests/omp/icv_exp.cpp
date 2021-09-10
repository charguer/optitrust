#include <stdio.h>

#include <omp.h>

int main() {
  omp_set_nested(1);
  omp_set_max_active_levels(8);
  omp_set_dynamic(0);
  omp_set_num_threads(2);
#pragma omp parallel
  {
    omp_set_num_threads(3);
#pragma omp parallel
    {
      omp_set_num_threads(4);
#pragma omp single
      {
        printf("Inner: max_act_lev=%d, num_thds=%d, max_thds=%d\n",
               omp_get_max_active_levels(), omp_get_num_threads(),
               omp_get_max_threads());
      }
    }
#pragma omp barrier
#pragma omp single
    {
      {
        printf("Outer: max_act_lev=%d, num_thds=%d, max_thds=%d\n",
               omp_get_max_active_levels(), omp_get_num_threads(),
               omp_get_max_threads());
      }
    }
  }
  return 0;
}