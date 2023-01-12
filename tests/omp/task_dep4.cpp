#include <omp.h>
#include <stdio.h>

int main(){
  int x = 1;

  {
    x = 2;
    printf("x + 1 = %d. ", x + 1);
    printf("x + 2 = %d\n ", x + 2);

  }
  return 0;
}