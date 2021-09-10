#include <stdio.h>

#include <omp.h>

int main() {
  int x;
  x = 2;
#pragma omp parallel num_threads(3) shared(x)
  {
    if ((omp_get_thread_num() == 0)) {
      x = 5;
    } else {
      int xval;
      xval = x;
      printf("1: Thread# %d: x = %d\n", omp_get_thread_num(), xval);
    }
#pragma omp barrier
    if ((omp_get_thread_num() == 0)) {
      printf("2: Thread# %d: x = %d\n", omp_get_thread_num(), x);
    } else {
      printf("3: Thread# %d: x = %d\n", omp_get_thread_num(), x);
    }
  }
  return 0;
}