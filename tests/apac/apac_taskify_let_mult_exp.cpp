#include <stdlib.h>

void f(int* tab) { tab[0] += 42; }

void g(const int* tab) {}

void h(const int* const tab) {}

void p(int* v) {
  int a = 15;
  int b = a + 2;
  int c = a + b + (*v)++;
}

void foo(int* bar, int val) {
#pragma omp taskgroup
  {
    int i;
    int j = 0;
    int* a;
    int** b;
    const int* c;
    const int** d;
    const int* const e = NULL;
#pragma omp task default(shared) depend(in : bar) depend(inout : bar[0])
    f(bar);
    for (i = 0; i < val; i++) {
#pragma omp taskwait depend(in : bar, i) depend(inout : bar[i])
      bar[i] += 2;
#pragma omp task default(shared) depend(in : a) depend(inout : a[i]) firstprivate(i)
      {
        p(&a[i]);
        p(&a[i]);
      }
    }
#pragma omp task default(shared) depend(in : c[0], c)
    g(c);
#pragma omp task default(shared) depend(in : e[0], e)
    h(e);
  __apac_exit:;
  }
}
