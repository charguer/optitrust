#include <omp.h>
#include <stdlib.h>
const static int __apac_depth_infinite = getenv("APAC_TASK_DEPTH_INFINITE") ? 1 : 0;

const static int __apac_depth_max = getenv("APAC_TASK_DEPTH_MAX") ? atoi(getenv("APAC_TASK_DEPTH_MAX")) : 5;

int __apac_depth = 0;

#pragma omp threadprivate(__apac_depth)

void f(int* tab) { tab[0] += 42; }

void g(const int* tab) {}

void h(const int* tab) {}

void p(int* v) {
  int a = 15;
  int b = a + 2;
  int c = a + b + (*v)++;
}

void c_seq(int* tab, int size) {
  f(tab);
  int i;
  for (i = 0; i < size; i++) {
    tab[i] += 2;
    p(&tab[i]);
    p(&tab[i]);
  }
  g(tab);
  h(tab);
}

void c(int* tab, int size) {
  int __apac_depth_local = __apac_depth;
  int __apac_depth_ok = __apac_depth_infinite || __apac_depth_local < __apac_depth_max;
  if (__apac_depth_ok) {
#pragma omp taskgroup
    {
#pragma omp task default(shared) depend(in : tab) depend(inout : tab[0]) firstprivate(__apac_depth_local) if (__apac_depth_ok)
      {
        if (__apac_depth_ok) {
          __apac_depth = __apac_depth_local + 1;
        }
        f(tab);
      }
      int i;
      for (i = 0; i < size; i++) {
#pragma omp task default(shared) depend(in : tab) depend(inout : tab[i]) firstprivate(__apac_depth_local, i) if (__apac_depth_ok)
        {
          if (__apac_depth_ok) {
            __apac_depth = __apac_depth_local + 1;
          }
          tab[i] += 2;
          p(&tab[i]);
          p(&tab[i]);
        }
      }
#pragma omp task default(shared) depend(in : tab, tab[0]) firstprivate(__apac_depth_local) if (__apac_depth_ok)
      {
        if (__apac_depth_ok) {
          __apac_depth = __apac_depth_local + 1;
        }
        g(tab);
      }
#pragma omp task default(shared) depend(in : tab, tab[0]) firstprivate(__apac_depth_local) if (__apac_depth_ok)
      {
        if (__apac_depth_ok) {
          __apac_depth = __apac_depth_local + 1;
        }
        h(tab);
      }
    __apac_exit:;
    }
  } else {
    c_seq(tab, size);
  }
}

int main_ser() {
  int* t = (int*)malloc(4 * sizeof(int));
  c(t, 4);
  free(t);
  return 0;
}

int main() {
  int __apac_depth_local = __apac_depth;
  int __apac_depth_ok = __apac_depth_infinite || __apac_depth_local < __apac_depth_max;
  if (__apac_depth_ok) {
    int __apac_result;
#pragma omp parallel
#pragma omp master
#pragma omp taskgroup
    {
      int* t;
#pragma omp task default(shared) depend(inout : t, t[0]) firstprivate(__apac_depth_local) if (__apac_depth_ok)
      {
        if (__apac_depth_ok) {
          __apac_depth = __apac_depth_local + 1;
        }
        t = (int*)malloc(4 * sizeof(int));
        c(t, 4);
        free(t);
      }
      __apac_result = 0;
      goto __apac_exit;
    __apac_exit:;
    }
    return __apac_result;
  } else {
    return main_ser();
  }
}
