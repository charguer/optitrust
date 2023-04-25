#include <omp.h>

void work();

int main() {
#pragma omp parallel proc_bind(close) num_threads(4)
  { work(); }
  return 0;
}
