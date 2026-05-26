#include <stdlib.h>

int f(int a, int b) { return a + b; }

int g(int* a) {
  *a = 2;
  return 0;
}

int h() {
  int __apac_result;
#pragma omp taskgroup
  {
    int a;
    int b;
    b = 0;
    b = b + 1;
    b--;
    int** c;
    c = (int**)malloc(sizeof(int));
    a = 1 + b++;
    b++;
#pragma omp task default(shared) depend(in : c, c[0]) depend(inout : c[0][0])
    g(*c);
    a = 2;
#pragma omp task default(shared) depend(in : b) depend(inout : a)
    {
      f(a, b);
      a = 3;
    }
#pragma omp taskwait
    **c = a;
  __apac_exit:;
  }
  return __apac_result;
}
