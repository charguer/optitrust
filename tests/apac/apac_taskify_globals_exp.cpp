#include <stdio.h>

int i;

int j;

void bar(int* a) { (*a)++; }

void rab(int a) { a++; }

void oof() { printf("%d\n", j); }

void func() {
#pragma omp critical
  {
    i += 1;
    rab(i++);
  }
}

void foo() {
#pragma omp taskgroup
  {
#pragma omp task default(shared)
    func();
#pragma omp task default(shared)
    func();
#pragma omp task default(shared) depend(in : j) depend(inout : i)
    {
#pragma omp critical
      {
        bar(&i);
        i--;
        i = j + 5;
      }
    }
#pragma omp taskwait
  __apac_exit:;
  }
}
