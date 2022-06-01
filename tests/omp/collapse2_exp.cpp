#include <stdio.h>

void test() {
#pragma omp parallel
  int jlast;
  int klast;
  {
#pragma omp for collapse(2) lastprivate(jlast, klast)
    for (int k = 1; k <= 2; k++)
      for (int j = 1; j <= 3; j++) {
        jlast = j;
        klast = k;
      }
#pragma omp single
    printf("%d %d\n", klast, jlast);
  }
}
