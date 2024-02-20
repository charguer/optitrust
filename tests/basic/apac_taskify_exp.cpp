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
  /*@__apac_task_group*/ #pragma omp taskgroup {
    int i;
#pragma omp task default(shared) depend(inout : *tab)
    f(tab);
#pragma omp task default(shared) depend(in : size) depend(inout : i, tab[*i])
    for (i = 0; i < size; i++) {
#pragma omp task default(shared) depend(inout : tab[*i])
      {
        tab[i] += 2;
        p(tab[i]);
      }
    }
#pragma omp task default(shared) depend(in : *tab)
    h(tab);
#pragma omp task default(shared) depend(in : *tab)
    g(tab);
  __apac_exit:;
  } /*__apac_task_group@*/
}

int main() {
  int* t = (int*)malloc(4 * sizeof(int));
  c(t, 4);
  free(t);
  return 0;
}
