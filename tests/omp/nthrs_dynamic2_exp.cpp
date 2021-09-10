#include <omp.h>

int main() {
  omp_set_dynamic(1);
#pragma omp parallel num_threads(10)
  {}
  return 0;
}