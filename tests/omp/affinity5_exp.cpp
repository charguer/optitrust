#include <omp.h>

void work();

int main() {
#pragma omp parallel proc_bind(master) num_threads(4)
  { work(); }
  return 0;
}
