#include <stdlib.h>

int f(const int a, const int b) { return a + b; }

int g(int* a) {
  *a = 2;
  return 0;
}

int h() {
  int __res;
  /*@__apac_task_group*/ #pragma omp taskgroup {
    int** c;
    int b;
    int a;
#pragma omp task default(shared) depend(inout : c)
    {
      c = (int**)malloc(sizeof(int));
      g(*c);
    }
#pragma omp task default(shared) depend(inout : b)
    {
      b = 0;
      b = b + 1;
      b--;
    }
#pragma omp task default(shared) depend(inout : a, b)
    a = 1 + b++;
#pragma omp task default(shared) depend(inout : b)
    b++;
#pragma omp task default(shared) depend(inout : a)
    a = 2;
#pragma omp task default(shared) depend(in : a, b)
    f(a, b);
#pragma omp task default(shared) depend(inout : a)
    a = 3;
#pragma omp task default(shared) depend(in : a) depend(inout : **c)
    **c = a;
  __apac_exit:;
  } /*__apac_task_group@*/
  return __res;
}
