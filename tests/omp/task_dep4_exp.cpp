#include <omp.h>
#include <stdio.h>

int main() {
  int x = 1;
#pragma omp parallel
#pragma omp single
  {
#pragma omp task shared(x) depend(out : x)
    x = 2;
#pragma omp task shared(x) depend(in : x)
    printf("x + 1 = %d. ", x + 1);
#pragma omp task shared(x) depend(in : x)
    printf("x + 2 = %d\n ", x + 2);
  }
  return 0;
}
