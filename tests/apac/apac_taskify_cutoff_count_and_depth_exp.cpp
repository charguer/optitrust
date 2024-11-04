#include <omp.h>
#include <stdlib.h>
const static int __apac_count_infinite = getenv("APAC_TASK_COUNT_INFINITE") ? 1 : 0;

const static int __apac_depth_infinite = getenv("APAC_TASK_DEPTH_INFINITE") ? 1 : 0;

const static int __apac_count_max = getenv("APAC_TASK_COUNT_MAX") ? atoi(getenv("APAC_TASK_COUNT_MAX")) : omp_get_max_threads() * 10;

const static int __apac_depth_max = getenv("APAC_TASK_DEPTH_MAX") ? atoi(getenv("APAC_TASK_DEPTH_MAX")) : 5;

int __apac_count = 0;

int __apac_depth = 0;

#pragma omp threadprivate(__apac_depth)

void f(int* tab) { tab[0] += 42; }

void g(const int* tab) {}

void h(const int* tab) {}

void p(int& v) {
  int a = 15;
  int b = a + 2;
  int c = a + b + v++;
}

void c(int* tab, int size) {
#pragma omp taskgroup
  {
    int __apac_count_ok = __apac_count_infinite || __apac_count < __apac_count_max;
    int __apac_depth_local = __apac_depth;
    int __apac_depth_ok = __apac_depth_infinite || __apac_depth_local < __apac_depth_max;
    if (__apac_count_ok) {
#pragma omp atomic
      __apac_count++;
    }
#pragma omp task default(shared) depend(in : tab) depend(inout : tab[0]) firstprivate(__apac_depth_local) if (__apac_count_ok || __apac_depth_ok)
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
    int i;
    for (i = 0; i < size; i++) {
      if (__apac_count_ok) {
#pragma omp atomic
        __apac_count++;
      }
#pragma omp task default(shared) depend(in : tab) depend(inout : tab[i]) firstprivate(__apac_depth_local, i) if (__apac_count_ok || __apac_depth_ok)
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
#pragma omp task default(shared) depend(in : tab, tab[0]) firstprivate(__apac_depth_local) if (__apac_count_ok || __apac_depth_ok)
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
    if (__apac_count_ok) {
#pragma omp atomic
      __apac_count++;
    }
#pragma omp task default(shared) depend(in : tab, tab[0]) firstprivate(__apac_depth_local) if (__apac_count_ok || __apac_depth_ok)
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
  __apac_exit:;
  }
}

int main() {
  int __apac_result;
#pragma omp parallel
#pragma omp master
#pragma omp taskgroup
  {
    int __apac_count_ok = __apac_count_infinite || __apac_count < __apac_count_max;
    int __apac_depth_local = __apac_depth;
    int __apac_depth_ok = __apac_depth_infinite || __apac_depth_local < __apac_depth_max;
    int* t;
    if (__apac_count_ok) {
#pragma omp atomic
      __apac_count++;
    }
#pragma omp task default(shared) depend(inout : t, t[0]) firstprivate(__apac_depth_local) if (__apac_count_ok || __apac_depth_ok)
    {
      if (__apac_count_ok || __apac_depth_ok) {
        __apac_depth = __apac_depth_local + 1;
      }
      t = (int*)malloc(4 * sizeof(int));
      c(t, 4);
      free(t);
      if (__apac_count_ok) {
#pragma omp atomic
        __apac_count--;
      }
    }
    __apac_result = 0;
    goto __apac_exit;
  __apac_exit:;
  }
  return __apac_result;
}
