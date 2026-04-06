#include <optitrust_models.h>

void f(int* t, int* u) {
  __requires("T: int -> int");
  __writes("u ~> Matrix1(10, T)");
  __reads("t ~> Matrix1(10, T)");
  int* const t2 = (int*)malloc(MSIZE1(10) * sizeof(int));
  for (int i = 0; i < 10; i++) {
    __strict();
    __xwrites("&t2[MINDEX1(10, i)] ~~> T(i)");
    __xreads("&t[MINDEX1(10, i)] ~~> T(i)");
    t2[MINDEX1(10, i)] = t[MINDEX1(10, i)];
  }
  for (int i = 0; i < 10; i++) {
    __strict();
    __xwrites("&u[MINDEX1(10, i)] ~~> T(i)");
    __xreads("&t2[MINDEX1(10, i)] ~~> T(i)");
    int x = t2[MINDEX1(10, i)];
    u[MINDEX1(10, i)] = x;
    int z = x;
  }
  free(t2);
  __ghost(ro_matrix1_focus, "matrix := t, i := 0");
  int* const t02 = (int*)malloc(MSIZE0() * sizeof(int));
  t02[MINDEX0()] = t[MINDEX1(10, 0)];
  for (int l = 0; l < 5; l++) {
    __strict();
    __sreads("&t02[MINDEX0()] ~~> T(0)");
    for (int m = 0; m < 2; m++) {
      __strict();
      __sreads("&t02[MINDEX0()] ~~> T(0)");
      int x = l + m + t02[MINDEX0()];
    }
  }
  free(t02);
  __ghost(ro_matrix1_unfocus, "matrix := t");
  int* const a2 = (int*)malloc(MSIZE1(8) * sizeof(int));
  for (int a = 0; a < 8; a++) {
    __strict();
    __xwrites("&a2[MINDEX1(8, a)] ~~> a");
    a2[MINDEX1(8, a)] = a;
  }
  int* const u2 = (int*)malloc(MSIZE1(8) * sizeof(int));
  for (int a = 0; a < 8; a++) {
    __strict();
    __sreads("u ~> Matrix1(10, T)");
    __xwrites("&u2[MINDEX1(8, a)] ~~> T(0)");
    const __ghost_fn __ghost_pair_8 =
        __ghost_begin(ro_matrix1_focus, "matrix := u, i := 0");
    u2[MINDEX1(8, a)] = u[MINDEX1(10, 0)];
    __ghost_end(__ghost_pair_8);
  }
  for (int a = 0; a < 8; a++) {
    __strict();
    __xreads("&a2[MINDEX1(8, a)] ~~> a");
    __xreads("&u2[MINDEX1(8, a)] ~~> T(0)");
    int y1 = 0;
    int y2 = 1;
    for (int b = 0; b < 5; b++) {
      __strict();
      __sreads("&u2[MINDEX1(8, a)] ~~> T(0)");
      __sreads("&a2[MINDEX1(8, a)] ~~> a");
      int p1 = 0;
      int p2 = 1;
      for (int c = 0; c < 2; c++) {
        __strict();
        __sreads("&u2[MINDEX1(8, a)] ~~> T(0)");
        __sreads("&a2[MINDEX1(8, a)] ~~> a");
        int q1 = 0;
        int q2 = 1;
        int x = a2[MINDEX1(8, a)] + b + c + u2[MINDEX1(8, a)];
      }
      int z1 = 0;
      int z2 = 1;
    }
  }
  free(u2);
  free(a2);
}
