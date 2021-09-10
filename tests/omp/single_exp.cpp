#include <stdio.h>

void work1() {}

void work2() {}

void single_example() {
#pragma omp parallel
  {
#pragma omp single
    printf("Beginning work1.\n");
    work1();
#pragma omp single
    printf("Finishing work1.\n");
    work2();
#pragma omp single nowait
    printf("Finishing work2.\n");
  }
}