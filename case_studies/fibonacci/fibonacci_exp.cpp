#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
const static int __apac_count_infinite = getenv("APAC_TASK_COUNT_INFINITE") ? 1 : 0;

const static int __apac_depth_infinite = getenv("APAC_TASK_DEPTH_INFINITE") ? 1 : 0;

const static int __apac_count_max = getenv("APAC_TASK_COUNT_MAX") ? atoi(getenv("APAC_TASK_COUNT_MAX")) : omp_get_max_threads() * 10;

const static int __apac_depth_max = getenv("APAC_TASK_DEPTH_MAX") ? atoi(getenv("APAC_TASK_DEPTH_MAX")) : 5;

int __apac_count = 0;

int __apac_depth = 0;

#pragma omp threadprivate(__apac_depth)

long long int fibonacci(const int n) {
  long long int __apac_result;
#pragma omp taskgroup
  {
    int __apac_count_ok = __apac_count_infinite || __apac_count < __apac_count_max;
    int __apac_depth_local = __apac_depth;
    int __apac_depth_ok = __apac_depth_infinite || __apac_depth_local < __apac_depth_max;
    long long int x, y;
#pragma omp taskwait depend(in : n)
    if (n < 2) {
#pragma omp taskwait
      __apac_result = n;
      goto __apac_exit;
    }
    if (__apac_count_ok) {
#pragma omp atomic
      __apac_count++;
    }
#pragma omp task default(shared) depend(in : n) depend(inout : x) firstprivate(__apac_depth_local) if (__apac_count_ok || __apac_depth_ok)
    {
      if (__apac_count_ok || __apac_depth_ok) {
        __apac_depth = __apac_depth_local + 1;
      }
      x = fibonacci(n - 1);
      if (__apac_count_ok) {
#pragma omp atomic
        __apac_count--;
      }
    }
    if (__apac_count_ok) {
#pragma omp atomic
      __apac_count++;
    }
#pragma omp task default(shared) depend(in : n) depend(inout : y) firstprivate(__apac_depth_local) if (__apac_count_ok || __apac_depth_ok)
    {
      if (__apac_count_ok || __apac_depth_ok) {
        __apac_depth = __apac_depth_local + 1;
      }
      y = fibonacci(n - 2);
      if (__apac_count_ok) {
#pragma omp atomic
        __apac_count--;
      }
    }
#pragma omp taskwait
    __apac_result = x + y;
    goto __apac_exit;
  __apac_exit:;
  }
  return __apac_result;
}

int main(const int argc, char** argv) {
  int __apac_result;
#pragma omp parallel
#pragma omp master
#pragma omp taskgroup
  {
    int __apac_count_ok = __apac_count_infinite || __apac_count < __apac_count_max;
    int __apac_depth_local = __apac_depth;
    int __apac_depth_ok = __apac_depth_infinite || __apac_depth_local < __apac_depth_max;
    int n = 16;
#pragma omp taskwait
    if (argc > 1) {
      n = atoi(argv[1]);
    }
    long long int result;
    if (__apac_count_ok) {
#pragma omp atomic
      __apac_count++;
    }
#pragma omp task default(shared) depend(in : n) depend(inout : result) firstprivate(__apac_depth_local) if (__apac_count_ok || __apac_depth_ok)
    {
      if (__apac_count_ok || __apac_depth_ok) {
        __apac_depth = __apac_depth_local + 1;
      }
      result = fibonacci(n);
      if (__apac_count_ok) {
#pragma omp atomic
        __apac_count--;
      }
    }
#pragma omp taskwait
    printf("fibonacci(%d) = %lld\n", n, result);
    __apac_result = 0;
    goto __apac_exit;
  __apac_exit:;
  }
  return __apac_result;
}
