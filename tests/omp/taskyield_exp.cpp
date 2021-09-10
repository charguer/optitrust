#include <omp.h>

void something_useful();

void something_critical();

void foo(omp_lock_t *lock, int n) {
  int i;
  for (int i = 0; (i < n); i++) {
    something_useful();
    while ((!omp_test_lock(lock))) {
#pragma omp taskyield
    }
    something_critical();
    omp_unset_lock(&lock);
  }
}