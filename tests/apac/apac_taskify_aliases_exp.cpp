#include <stdlib.h>

int** f(int** v) {
  v = (int**)malloc(sizeof(int*));
  return v;
}

int* g(int* v) {
  v = (int*)malloc(sizeof(int));
  return v;
}

int k() { return 42; }

int h(int** v1, int* v2, int v3) {
  int __apac_result;
#pragma omp taskgroup
  {
#pragma omp task default(shared) depend(inout : v1, v1[0], v1[0][0])
    v1 = f(v1);
#pragma omp taskwait depend(in : v1)
    int** a1 = v1;
#pragma omp task default(shared) depend(inout : v1)
    a1[0] = g(a1[0]);
#pragma omp taskwait depend(in : v1)
    int* a2 = a1[0];
    int* a2bis = NULL;
#pragma omp task default(shared) depend(inout : v1)
    a2[0] = k();
#pragma omp task default(shared) depend(inout : v2)
    {
      a2bis = v2;
      a2 = v2 + 1;
      a2[0] = k();
      a2bis[0] = k();
    }
    int a3 = v3;
    a3++;
    __apac_result = a3;
    goto __apac_exit;
  __apac_exit:;
  }
  return __apac_result;
}
