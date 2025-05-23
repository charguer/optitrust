#include <optitrust.h>

void f(int *t, int *u) {
  __reads("for i in 0..10 -> &t[i] ~> Cell");
  __modifies("for i in 0..10 -> &u[i] ~> Cell");

  for (int i = 0; i < 10; i++) {
    __strict();
    __xreads("&t[i] ~> Cell");
    __xmodifies("&u[i] ~> Cell");

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
      __strict();
      __xmodifies("&u[m] ~> Cell");

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
  // - int* y_step = MALLOC1(int, 2);
  // - becomes:
  //   - int* y_step_step = MALLOC2(int, 5, 2);
  // vs:
  // - int* y_step;
  //   y_step = MALLOC1(int, 2);
  // - becomes:
  //   - int* y_step_step = MALLOC1(int*, 5);
}

void f2(float* A, float* B, int m, int n, int p) {
  __reads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");

  for (int i = 0; i < m; i++) {
    __strict();
    __sreads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");

    for (int j = 0; j < n; j++) {
      __strict();
      __sreads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");

      float sum;
      sum = 0.0f;
      for (int k = 0; k < p; k++) {
        __ghost(ro_matrix2_focus, "A, i, k");
        __ghost(ro_matrix2_focus, "B, k, j");
        sum += A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
        __ghost(ro_matrix2_unfocus, "A");
        __ghost(ro_matrix2_unfocus, "B");
      }
      sum++;
    }
  }
}
