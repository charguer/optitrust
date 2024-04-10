#include <stdlib.h>

void f(int* tab) { tab[0] += 42; }

void g(const int* const tab) {}

void h(const int* const tab) {}

void p(int v) {
  int a = 15;
  int b = a + 2;
  int c = a + b + v++;
}

void r(int v, const int z) { int a = 15 + z, b = a + 2, c = a + b + v++; }

void c(int* tab, const int size) {
#pragma omp taskgroup
  {
    int i;
#pragma omp task default(shared) depend(inout : tab[0])
    f(tab);
#pragma omp taskwait depend(in : size) depend(inout : i)
    for (i = 0; i < size; i++) {
#pragma omp taskwait depend(in : i)
#pragma omp task default(shared) depend(inout : tab[i]) firstprivate(i)
      {
        tab[i] += 2;
        p(tab[i]);
        p(tab[i]);
      }
    }
#pragma omp task default(shared) depend(in : tab[0])
    h(tab);
#pragma omp task default(shared) depend(in : tab[0])
    g(tab);
  __apac_exit:;
  }
}

int main() {
  int* t = (int*)malloc(4 * sizeof(int));
  c(t, 4);
  free(t);
  return 0;
}
