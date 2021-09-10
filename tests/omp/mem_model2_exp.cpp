#include <omp.h>

#include <stdio.h>

int main() {
  int data;
  int flag = 0;
#pragma omp parallel num_threads(2)
  {
#pragma omp barrier
    if ((omp_get_thread_num() == 0)) {
      data = 42;
#pragma omp flush(flag, data)
      flag = 1;
#pragma omp flush(flag)
    } else if ((omp_get_thread_num() == 1)) {
      int flag_val = 0;
#pragma omp flush(flag, data)
      while ((flag_val < 1)) {
#pragma omp flush(flag, data)
        flag_val = flag;
      }
#pragma omp flush(flag, data)
      printf("flag=%d data=%d\n", flag, data);
      printf("flag=%d data=%d\n", flag, data);
    }
  }
  return 0;
}