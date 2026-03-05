#include <optitrust_models.h>

void f(int* t, int* u) {
  __requires("T: int -> int");
  __reads("t ~> Matrix1(10, T)");
  __writes("u ~> Matrix1(10, T)");

  for (int i = 0; i < 10; i++) {
    __strict();
    __xreads("&t[MINDEX1(10, i)] ~~> T(i)");
    __xwrites("&u[MINDEX1(10, i)] ~~> T(i)");

    int x = t[MINDEX1(10, i)];
    u[MINDEX1(10, i)] = x;
    int z = x;
  }

  __ghost(ro_matrix1_focus, "t, 0");
  for (int l = 0; l < 5; l++) {
    __strict();
    __sreads("&t[MINDEX1(10, 0)] ~~> T(0)");

    for (int m = 0; m < 2; m++) {
      __strict();
      __sreads("&t[MINDEX1(10, 0)] ~~> T(0)");

      int x = l + m + t[MINDEX1(10, 0)];
    }
  }
  __ghost(ro_matrix1_unfocus, "t");

  for (int a = 0; a < 8; a++) {
    __strict();
    __sreads("u ~> Matrix1(10, T)");

    int y1 = 0;
    int y2 = 1;
    for (int b = 0; b < 5; b++) {
      __strict();
      __sreads("u ~> Matrix1(10, T)");
      int p1 = 0;
      int p2 = 1;

      for (int c = 0; c < 2; c++) {
        __strict();
        __sreads("u ~> Matrix1(10, T)");
        int q1 = 0;
        int q2 = 1;

        // TODO: ideally, figure out some ghost to put in the rewrite path here
        __GHOST_BEGIN(focus, ro_matrix1_focus, "u, 0");
        int x = a + b + c + u[MINDEX1(10, 0)];
        __GHOST_END(focus);
      }
      int z1 = 0;
      int z2 = 1;
    }
  }
}
