#include <stdio.h>
#include <optitrust.h>

void matrix2_copy(int* D, int* S) {
  __modifies("D ~> Matrix2(1024,1024)");
  __reads("S ~> Matrix2(1024,1024)");

  for (int ci = 0; ci < 2; ++ci) {
    __strict();
    __sreads("S ~> Matrix2(1024,1024)");
    __xmodifies("for cj in range(ci, 256, 2) -> &D[MINDEX2(1024, 1024, ,)] ~> Cell");
    // __GHOST_BEGIN(focus, ro_matrix1_focus, "S, k");
    for (int cj = 0; cj < 2; ++cj) {
      for (int bi = 0; bi < 256; bi+=2) {
        for (int bj = 0; bj < 256; bj+=2) {
          for (int i = 0; i < 4; ++i) {
            for (int j = 0; j < 4; ++j) {
              D[MINDEX2(1024, 1024, bi*4 + i, bj*4 + j)] = S[MINDEX2(1024, 1024, bi*4 + i, bj*4 + j)];
            }
          }
        }
      }
    }
  }
}
