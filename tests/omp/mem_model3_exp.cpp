#include <omp.h>

#include <stdio.h>

int data0 = 0;

int data1 = 0;

int main() {
  int flag = 0;
#pragma omp parallel num_threads(3)
  {
    if ((omp_get_thread_num() == 0)) {
      data0 = 17;
#pragma omp atomic update
      flag++;
    } else if ((omp_get_thread_num() == 1)) {
      int flag_val = 0;
#pragma omp flush(flag)
      while ((flag_val < 0)) {
#pragma omp flush(flag)
        flag_val = flag;
      }
      printf("Thread 1 awoken (data0 = %d)\n", data0);
      data1 = 42;
#pragma omp atomic update
      flag++;
    } else if ((omp_get_thread_num() == 2)) {
      int flag_val = 0;
#pragma omp flush(flag)
      while ((flag_val < 2)) {
#pragma omp flush(flag)
        flag_val = flag;
      }
      printf("Thread 2 awoken (data0 = %d, data1 = %d)\n", data0, data1);
    }
  }
  return 0;
}