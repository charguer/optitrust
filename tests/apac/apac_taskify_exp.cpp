#include <stdlib.h>

void f(int* tab) { tab[0] += 42; }

void g(const int* tab) {}

void h(const int* tab) {}

void p(int* v) {
  int a = 15;
  int b = a + 2;
  int c = a + b + (*v)++;
}

void r(int v, int z) {
  int a = 15 + z;
  int b = a + 2;
  int c = a + b + v++;
}

int w(int a) { return a + 1; }

void c(int* tab, int size) {
#pragma omp taskgroup
  {
    int i;
    int l = size;
    int r;
    int s = 1;
#pragma omp task default(shared) depend(in : tab) depend(inout : tab[0])
    f(tab);
    for (i = 0; i < size; i++) {
#pragma omp task default(shared) depend(in : tab) depend(inout : tab[i]) firstprivate(i)
      {
        tab[i] += 2;
        p(&tab[i]);
        p(&tab[i]);
      }
    }
    int* p = tab;
    while (l > 1 || s) {
#pragma omp task default(shared) depend(in : l) depend(inout : r)
      r = w(l);
#pragma omp taskwait depend(in : r) depend(inout : tab)
      *p = r;
      p++;
#pragma omp taskwait depend(in : r) depend(inout : l)
      l /= r;
      s = 0;
    }
#pragma omp task default(shared) depend(in : tab, tab[0])
    g(tab);
#pragma omp task default(shared) depend(in : tab, tab[0])
    h(tab);
  __apac_exit:;
  }
}

int main() {
  int __apac_result;
#pragma omp parallel
#pragma omp master
#pragma omp taskgroup
  {
    int* t = (int*)malloc(4 * sizeof(int));
    c(t, 4);
    free(t);
    __apac_result = 0;
    goto __apac_exit;
  __apac_exit:;
  }
  return __apac_result;
}
