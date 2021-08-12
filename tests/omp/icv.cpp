#include <stdio.h>
#include <omp.h>

int main (void){

  // The following should print:
  // Inner: max_act_lev=8, num_thds=3, max_thds=4
  // Inner: max_act_lev=8, num_thds=3, max_thds=4
  printf ("Inner: max_act_lev=%d, num_thds=%d, max_thds=%d\n",
  omp_get_max_active_levels(), omp_get_num_threads(),
  omp_get_max_threads());

  // The following should print:
  // Outer: max_act_lev=8, num_thds=2, max_thds=3
  printf ("Outer: max_act_lev=%d, num_thds=%d, max_thds=%d\n",
  omp_get_max_active_levels(), omp_get_num_threads(),
  omp_get_max_threads());
  return 0;
}