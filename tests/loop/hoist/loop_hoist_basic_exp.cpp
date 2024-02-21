#include <optitrust.h>



  void f ( int* t, int* u )  {
  __modifies("Group(range(0, 10, 1), fun i -> &u[i] ~> Cell)");
  __reads("Group(range(0, 10, 1), fun i -> &t[i] ~> Cell)");
  int*   const  x_step = ( int* const ) MALLOC1(10, sizeof(int));
  int*   const  z_step = ( int* const ) MALLOC1(10, sizeof(int));
  for ( int i = 0; i < 10; i++ ) {
    __modifies("_Uninit(&z_step[MINDEX1(10, i)] ~> Cell)");
    __modifies("_Uninit(&x_step[MINDEX1(10, i)] ~> Cell)");
    __modifies("&u[i] ~> Cell");
    __reads("&t[i] ~> Cell");
    int*   const  x = &x_step[MINDEX1(10, i)];
    x[MINDEX0()] = t[i];
    u[i] = x[MINDEX0()];
    int*   const  z = &z_step[MINDEX1(10, i)];
    z[MINDEX0()] = x[MINDEX0()];
    int w = 0;
  }
  MFREE1(10, z_step);
  MFREE1(10, x_step);
  for ( int l = 0; l < 5; l++ ) {
    const  __ghost_fn focus2_6 = __ghost_begin(group_focus_subrange, "start := 2, stop := 6");
    for ( int m = 2; m < 6; m++ ) {
            __modifies("&u[m] ~> Cell");
      for ( int n = 4; n < 11; n += 2 ) {
        int*   const  y = ( int* const ) MALLOC0(sizeof(int));
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
    __parallel_reads("A ~> Matrix2(m, p)");
    __parallel_reads("B ~> Matrix2(p, n)");
    float* const sum_step = (float* const)MALLOC1(m, sizeof(float));
    for (int j = 0; j < m; j++) {
      __parallel_reads("A ~> Matrix2(m, p)");
      __parallel_reads("B ~> Matrix2(p, n)");
      __modifies("_Uninit(&sum_step[MINDEX1(m, j)] ~> Cell)");
      float* const sum = &sum_step[MINDEX1(m, j)];
      sum[MINDEX0()] = 0.f;
      for (int k = 0; k < p; k++) {
        __ghost(matrix2_ro_focus, "M := A, i := i, j := k");
        __ghost(matrix2_ro_focus, "M := B, i := k, j := j");
        sum[MINDEX0()] += A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
        __ghost(matrix2_ro_unfocus, "M := A");
        __ghost(matrix2_ro_unfocus, "M := B");
      }
      sum[MINDEX0()]++;
    }
    MFREE1(m, sum_step);
  }
}
