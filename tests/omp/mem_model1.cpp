#include <stdio.h>
#include <omp.h>

int main(){
  int x;

  x = 2;
  {

    if (omp_get_thread_num() == 0) {
       x = 5;
    } else {
      int xval;
      xval = x;
    /* Print 1: xval can be 2 or 5 */
      printf("1: Thread# %d: x = %d\n", omp_get_thread_num(), xval);
    }


    if (omp_get_thread_num() == 0) {
    /* Print 2 */
      printf("2: Thread# %d: x = %d\n", omp_get_thread_num(), x);
    } else {
    /* Print 3 */
      printf("3: Thread# %d: x = %d\n", omp_get_thread_num(), x);
    }
  }
  return 0;
}