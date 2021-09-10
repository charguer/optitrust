#include <omp.h>

#include <stdio.h>

void work() {
#pragma omp task
  omp_lock_t lock;
  omp_init_lock(&lock);
#pragma omp parallel
  {
#pragma omp for
    for (int i = 0; (i < 100); i++) {
#pragma omp task
      {
        omp_set_lock(&lock);
#pragma omp task
        { printf("do work here\n"); }
        omp_unset_lock(&lock);
      }
    }
  }
  omp_destroy_lock(&lock);
}