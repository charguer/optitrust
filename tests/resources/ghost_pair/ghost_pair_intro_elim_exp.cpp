#include <optitrust.h>

void f() {
  __pure();
  int x = 0;
  int* const A = (int*)calloc(MSIZE1(10), sizeof(int));
  __ghost(matrix1_ro_focus, "matrix := A, i := 0");
  x += A[MINDEX1(10, 0)];
  __ghost(matrix1_ro_unfocus, "matrix := A, i := 0");
  free(A);
  int* const B = (int*)calloc(MSIZE2(8, 6), sizeof(int));
  __ghost(group_focus,
          "items := fun i -> for j in 0..6 -> &B[MINDEX2(8, 6, i, j)] ~> Cell, "
          "i := 1");
  __ghost(group_ro_focus,
          "items := fun j -> &B[MINDEX2(8, 6, 1, j)] ~> Cell, i := 2");
  x += B[MINDEX2(8, 6, 1, 2)];
  __ghost(group_ro_unfocus,
          "items := fun j -> &B[MINDEX2(8, 6, 1, j)] ~> Cell, i := 2");
  __ghost(group_unfocus,
          "items := fun i -> for j in 0..6 -> &B[MINDEX2(8, 6, i, j)] ~> Cell, "
          "i := 1");
  free(B);
}
