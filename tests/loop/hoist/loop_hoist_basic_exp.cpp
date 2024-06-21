#include <optitrust.h>

void f(int* t, int* u) {
  __modifies("for i in 0..10 -> &u[i] ~> Cell");
  __reads("for i in 0..10 -> &t[i] ~> Cell");
  int* const x_step = (int*)MALLOC1(10, sizeof(int));
  int* const z_step = (int*)MALLOC1(10, sizeof(int));
  for (int i = 0; i < 10; i++) {
    __strict();
    __xmodifies("_Uninit(&z_step[MINDEX1(10, i)] ~> Cell)");
    __xmodifies("_Uninit(&x_step[MINDEX1(10, i)] ~> Cell)");
    __xmodifies("&u[i] ~> Cell");
    __xreads("&t[i] ~> Cell");
    int* const x = &x_step[MINDEX1(10, i)];
    x[MINDEX0()] = t[i];
    u[i] = x[MINDEX0()];
    int* const z = &z_step[MINDEX1(10, i)];
    z[MINDEX0()] = x[MINDEX0()];
    int w = 0;
  }
  MFREE1(10, z_step);
  MFREE1(10, x_step);
  for (int l = 0; l < 5; l++) {
    __strict();
    __smodifies("for i in 0..10 -> &u[i] ~> Cell");
    const __ghost_fn focus2_6 =
        __ghost_begin(group_focus_subrange, "sub_range := 2..6");
    for (int m = 2; m < 6; m++) {
      __strict();
      __xmodifies("&u[m] ~> Cell");
      for (int n = 4; n < 11; n += 2) {
        __strict();
        __smodifies("&u[m] ~> Cell");
        int* const y = (int*)MALLOC0(sizeof(int));
        y[MINDEX0()] = 0;
        u[m] = y[MINDEX0()];
        MFREE0(y);
      }
    }
    __ghost_end(focus2_6);
  }
}

void f2(float* A, float* B, int m, int n, int p) {
  __reads("A ~> Matrix2(m, p)");
  __reads("B ~> Matrix2(p, n)");
  for (int i = 0; i < m; i++) {
    __strict();
    __sreads("A ~> Matrix2(m, p)");
    __sreads("B ~> Matrix2(p, n)");
    float* const sum_step = (float*)MALLOC1(n, sizeof(float));
    for (int j = 0; j < n; j++) {
      __strict();
      __sreads("A ~> Matrix2(m, p)");
      __sreads("B ~> Matrix2(p, n)");
      __xmodifies("_Uninit(&sum_step[MINDEX1(n, j)] ~> Cell)");
      float* const sum = &sum_step[MINDEX1(n, j)];
      sum[MINDEX0()] = 0.f;
      for (int k = 0; k < p; k++) {
        __strict();
        __smodifies("sum ~> Matrix0()");
        __sreads("A ~> Matrix2(m, p)");
        __sreads("B ~> Matrix2(p, n)");
        __ghost(matrix2_ro_focus, "M := A, i := i, j := k");
        __ghost(matrix2_ro_focus, "M := B, i := k, j := j");
        sum[MINDEX0()] += A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
        __ghost(matrix2_ro_unfocus, "M := A");
        __ghost(matrix2_ro_unfocus, "M := B");
      }
      sum[MINDEX0()]++;
    }
    MFREE1(n, sum_step);
  }
}
