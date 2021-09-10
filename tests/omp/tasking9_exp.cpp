#include <stdio.h>

void work() {
#pragma omp task
  {
#pragma omp task
    {
#pragma omp critical
      { printf("do work here \n"); }
    }
#pragma omp task
    {
#pragma omp critical
      { printf("do work here \n"); }
    }
  }
}