#include <stdlib.h>

int f(const int a, const int b) { return a + b; }

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
    int** c;
    b = 0;
    b = b + 1;
    b--;
#pragma omp taskwait
    c = (int**)malloc(sizeof(int));
#pragma omp taskwait depend(inout : a, b)
    a = 1 + b++;
#pragma omp task default(shared) depend(in : c, c[0]) depend(inout : c[0][0])
    g(*c);
#pragma omp taskwait depend(inout : a)
    a = 2;
#pragma omp taskwait depend(inout : b)
    b++;
#pragma omp task default(shared) depend(in : b) depend(inout : a)
    {
      f(a, b);
      a = 3;
    }
#pragma omp taskwait depend(in : a, c, c[0]) depend(inout : c[0][0])
    **c = a;
  __apac_exit:;
  }
  return __apac_result;
}
