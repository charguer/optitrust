#include <optitrust.h>

void f(int *t, int *u) {
  __reads("for i in 0..10 -> &t[i] ~> Cell");
  __modifies("for i in 0..10 -> &u[i] ~> Cell");

  for (int i = 0; i < 10; i++) {
    __reads("&t[i] ~> Cell");
    __modifies("&u[i] ~> Cell");

    int x;
    x = t[i];
    u[i] = x;
    int z;
    z = x;
    int w = 0;
  }

  for (int l = 0; l < 5; l++) {
    __GHOST_BEGIN(focus2_6, group_focus_subrange, "sub_range := 2..6");
    for (int m = 2; m < 6; m++) {
      __modifies("&u[m] ~> Cell");

      for (int n = 4; n < 11; n += 2) {
        int y;
        y = 0;
        u[m] = y;
      }
    }
    __GHOST_END(focus2_6);
  }

  // Question:
  // hoist:
  // - int* y_step = (int*) MALLOC1(2, sizeof(int));
  // - becomes:
  //   - int* y_step_step = (int*) MALLOC2(5, 2, sizeof(int));
  // vs:
  // - int* y_step;
  //   y_step = (int*) MALLOC1(2, sizeof(int));
  // - becomes:
  //   - int* y_step_step = (int**) MALLOC1(5, sizeof(int*));
}

void f2(float* A, float* B, int m, int n, int p) {
  __reads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");

  for (int i = 0; i < m; i++) {
    __parallel_reads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");

    for (int j = 0; j < n; j++) {
      __parallel_reads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");

      float sum;
      sum = 0.0f;
      for (int k = 0; k < p; k++) {
        __ghost(matrix2_ro_focus, "A, i, k");
        __ghost(matrix2_ro_focus, "B, k, j");
        sum += A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
        __ghost(matrix2_ro_unfocus, "A");
        __ghost(matrix2_ro_unfocus, "B");
      }
      sum++;
    }
  }
}
