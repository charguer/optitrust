#include <optitrust.h>

void matrix2_copy(int* D, int* S) {
  __modifies("D ~> Matrix2(1024,1024)");
  __reads("S ~> Matrix2(1024,1024)");

  for (int i = 0; i < 1024; i++) {
    __strict();
    __xmodifies("for j in 0..1024 -> &D[MINDEX2(1024, 1024, i, j)] ~> Cell");

    __sreads("S ~> Matrix2(1024,1024)");
    // __GHOST_BEGIN(focus, ro_matrix1_focus, "S, k");
    for (int j = 0; j < 1024; j++) {
      __strict();
      __xmodifies("&D[MINDEX2(1024, 1024, i, j)] ~> Cell");
      __sreads("S ~> Matrix2(1024,1024)");

      __GHOST_BEGIN(focus, ro_matrix2_focus, "S, i, j");
      D[MINDEX2(1024, 1024, i, j)] = S[MINDEX2(1024, 1024, i, j)];
      __GHOST_END(focus);
    }
  }
}
