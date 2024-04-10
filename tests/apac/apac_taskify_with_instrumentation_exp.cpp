#include <stdlib.h>

const static int __apac_count_infinite = getenv("APAC_TASK_COUNT_INFINITE") ? 1 : 0;

const static int __apac_depth_infinite = getenv("APAC_TASK_DEPTH_INFINITE") ? 1 : 0;

const static int __apac_count_max = getenv("APAC_TASK_COUNT_MAX") ? atoi(getenv("APAC_TASK_COUNT_MAX")) : omp_get_max_threads() * 10;

const static int __apac_depth_max = getenv("APAC_TASK_DEPTH_MAX") ? atoi(getenv("APAC_TASK_DEPTH_MAX")) : 5;

int __apac_count = 0;

int __apac_depth = 0;

#pragma omp threadprivate(__apac_depth)

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
    int __apac_count_ok = __apac_count_infinite || __apac_count < __apac_count_max;
    int __apac_depth_local = __apac_depth;
    int __apac_depth_ok = __apac_depth_infinite || __apac_depth_local < __apac_depth_max;
    int i;
    if (__apac_count_ok) {
#pragma omp atomic
      __apac_count++;
    }
#pragma omp task default(shared) depend(inout : tab[0]) if (__apac_count_ok || __apac_depth_ok)
    {
      if (__apac_count_ok || __apac_depth_ok) {
        __apac_depth = __apac_depth_local + 1;
      }
      f(tab);
      if (__apac_count_ok) {
#pragma omp atomic
        __apac_count--;
      }
    }
#pragma omp taskwait depend(in : size) depend(inout : i)
    for (i = 0; i < size; i++) {
      if (__apac_count_ok) {
#pragma omp atomic
        __apac_count++;
      }
#pragma omp taskwait depend(in : i)
#pragma omp task default(shared) depend(inout : tab[i]) firstprivate(i) if (__apac_count_ok || __apac_depth_ok)
      {
        if (__apac_count_ok || __apac_depth_ok) {
          __apac_depth = __apac_depth_local + 1;
        }
        tab[i] += 2;
        p(tab[i]);
        p(tab[i]);
        if (__apac_count_ok) {
#pragma omp atomic
          __apac_count--;
        }
      }
    }
    if (__apac_count_ok) {
#pragma omp atomic
      __apac_count++;
    }
#pragma omp task default(shared) depend(in : tab[0]) if (__apac_count_ok || __apac_depth_ok)
    {
      if (__apac_count_ok || __apac_depth_ok) {
        __apac_depth = __apac_depth_local + 1;
      }
      h(tab);
      if (__apac_count_ok) {
#pragma omp atomic
        __apac_count--;
      }
    }
    if (__apac_count_ok) {
#pragma omp atomic
      __apac_count++;
    }
#pragma omp task default(shared) depend(in : tab[0]) if (__apac_count_ok || __apac_depth_ok)
    {
      if (__apac_count_ok || __apac_depth_ok) {
        __apac_depth = __apac_depth_local + 1;
      }
      g(tab);
      if (__apac_count_ok) {
#pragma omp atomic
        __apac_count--;
      }
    }
  __apac_exit:;
  }
}

int main() {
  int* t = (int*)malloc(4 * sizeof(int));
  c(t, 4);
  free(t);
  return 0;
}
