#include <stdlib.h>

int f(const int a, const int b) { return a + b; }

int g(int* a) {
  *a = 2;
  return 0;
}

int h() {
  int __res;
  /*@__apac_task_group*/ #pragma omp taskgroup {
    int a;
  __exit:;
    int** c;
    c = (int**)malloc(sizeof(int));
    g(*c);
    int b;
    b = 0;
    b = b + 1;
    b--;
#pragma omp task default(shared) depend(inout : a, b)
    { a = 1 + b++; }
#pragma omp task default(shared) depend(inout : a)
    { a = 2; }
#pragma omp task default(shared) depend(inout : b)
    { b++; }
#pragma omp task default(shared) depend(in : a, b)
    { f(a, b); }
#pragma omp task default(shared) depend(inout : a)
    { a = 3; }
  } /*__apac_task_group@*/
  return __res;
}
