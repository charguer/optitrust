#include <stdio.h>

#include <assert.h>

int main() {
  int i, j;
  int *ptr_i, ptr_j;
  i = 1;
  j = 2;
  ptr_i = (&i);
  ptr_j = (&j);
#pragma omp parallel private(i) firstprivate(j)
  {
    i = 3;
    j = (j + 2);
  }
  return 0;
}