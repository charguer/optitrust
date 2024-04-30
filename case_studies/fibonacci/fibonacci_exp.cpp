#include <stdio.h>
#include <stdlib.h>

long long int fibonacci(const int n) {
  long long int __apac_result;
#pragma omp taskgroup
  {
    long long int x, y;
    if (n < 2) {
      __apac_result = n;
      goto __apac_exit;
    }
#pragma omp task default(shared) depend(in : n) depend(inout : x)
    x = fibonacci(n - 1);
#pragma omp task default(shared) depend(in : n) depend(inout : y)
    y = fibonacci(n - 2);
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
    int n = 16;
    long long int result;
    if (argc > 1) {
      n = atoi(argv[1]);
    }
#pragma omp task default(shared) depend(in : n) depend(inout : result)
    result = fibonacci(n);
#pragma omp taskwait
    printf("fibonacci(%d) = %lld\n", n, result);
    __apac_result = 0;
    goto __apac_exit;
  __apac_exit:;
  }
  return __apac_result;
}
