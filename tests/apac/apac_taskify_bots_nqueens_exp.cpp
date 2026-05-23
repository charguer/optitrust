#include <alloca.h>
#include <memory.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

#include "app-desc.hpp"
#include "bots.h"

int solutions[] = {1, 0, 0, 2, 10, 4, 40, 92, 352, 724, 2680, 14200, 73712, 365596};

int total_count;

int ok(int n, char* a) {
  int i;
  int j;
  char p;
  char q;
  for (i = 0; i < n; i++) {
    p = a[i];
    for (j = i + 1; j < n; j++) {
      q = a[j];
      if (q == p || q == p - (j - i) || q == p + (j - i)) return 0;
    }
  }
  return 1;
}

void nqueens_ser(int n, int j, char* a, int* solutions) {
  int res;
  int i;
  if (n == j) {
    *solutions = 1;
    return;
  }
  *solutions = 0;
  for (i = 0; i < n; i++) {
    a[j] = (char)i;
    if (ok(j + 1, a)) {
      nqueens_ser(n, j + 1, a, &res);
      *solutions += res;
    }
  }
}

void nqueens(int n, int j, const char* a, int* solutions) {
#pragma omp taskgroup
  {
    int* csols;
    int i;
    if (n == j) {
      *solutions = 1;
      goto __apac_exit;
    }
    *solutions = 0;
    csols = (int*)__builtin_alloca(n * sizeof(int));
    memset(csols, 0, n * sizeof(int));
    for (i = 0; i < n; i++) {
#pragma omp task default(shared) depend(in : a, a[0], csols, j, n) depend(inout : csols[i]) firstprivate(i)
      {
        char* b;
        b = (char*)malloc(n * sizeof(char));
        memcpy(b, a, j * sizeof(char));
        b[j] = (char)i;
        if (ok(j + 1, b)) {
          nqueens(n, j + 1, b, &csols[i]);
        }
        free(b);
      }
    }
#pragma omp taskwait
    for (i = 0; i < n; i++) {
      *solutions += csols[i];
    }
  __apac_exit:;
  }
}

void find_queens(int size) {
#pragma omp parallel
#pragma omp master
#pragma omp taskgroup
  {
    total_count = 0;
    bots_message("Computing N-Queens algorithm (n=%d) ", size);
    char* a;
    a = (char*)__builtin_alloca(size * sizeof(char));
    nqueens(size, 0, a, &total_count);
    bots_message(" completed!\n");
  __apac_exit:;
  }
}

int verify_queens(int size) {
  if (size > sizeof(solutions) / sizeof(int)) return 0;
  if (total_count == solutions[size - 1]) return 1;
  return 2;
}
