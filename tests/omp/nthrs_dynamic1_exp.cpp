#include <omp.h>
#include <stdio.h>

int main() {
  omp_set_dynamic(0);
#pragma omp parallel num_threads(10)
  { printf("do work here\n"); }
  return 0;
}
