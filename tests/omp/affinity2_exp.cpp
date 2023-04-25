#include <omp.h>

void work();

void foo() {
#pragma omp parallel num_threads(16) proc_bind(spread)
  { work(); }
}
