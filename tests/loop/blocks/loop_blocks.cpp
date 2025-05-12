#include <stdio.h>
#include <optitrust.h>

void matrix_copy(int* D, int* S) {
  __modifies("D ~> Matrix2(1024,1024)");
  __reads("S ~> Matrix2(1024,1024)");

   ..
  for (int k = 0; k < 1024; ++k) {
    __strict();
    __xmodifies("&D[MINDEX1(1024, k)] ~> Cell");
    __sreads("S ~> Matrix1(1024)");
    __GHOST_BEGIN(focus, ro_matrix1_focus, "S, k");
    D[MINDEX1(1024, k)] = S[MINDEX1(1024, k)];
    __GHOST_END(focus);
  }
}
