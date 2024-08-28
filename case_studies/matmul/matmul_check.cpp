#include <optitrust.h>

/* Multiplies the matrix A (dim m x p) by the matrix B (dim p x n),
 * and writes the result in the matrix C (dim m x n):
 *   C = A * B
 */
void mm(int m, int n, int p, float C[m][n], float A[m][p], float B[p][n]) {
  __reads("A, B");
  __modifies("C");

  for (int i = 0; i < m; i++) {
    __xmodifies("for j in 0..n -> &C[i][j] ~> Cell");

    for (int j = 0; j < n; j++) {
      __xmodifies("&C[i][j] ~> Cell");

      float sum = 0.0f;
      for (int k = 0; k < p; k++) {
        __GHOST_BEGIN(focusA, matrix2_ro_focus, "A, i, k");
        __GHOST_BEGIN(focusB, matrix2_ro_focus, "B, k, j");
        sum += A[i][k] * B[k][j];
        __GHOST_END(focusA);
        __GHOST_END(focusB);
      }

      C[i][j] = sum;
    }
  }
}

void mm1024(float C[1024][1024], float A[1024][1024], float B[1024][1024]) {
  __reads("A, B");
  __modifies("C");

  mm(1024, 1024, 1024, C, A, B);
}
