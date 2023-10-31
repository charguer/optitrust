#include "../../include/optitrust.h"

int main() {
  __pure();
  int x = 0;
  float* const A = (float* const)MALLOC1(10, sizeof(float));
  __ghost(matrix1_ro_focus, "M := A, i := 0");
  x += A[MINDEX1(10, 0)];
  __ghost(matrix1_ro_unfocus, "M := A, i := 0");
  MFREE1(10, A);
  float* const B = (float* const)MALLOC2(8, 6, sizeof(float));
  __ghost(group_focus,
          "items := fun i -> Group(range(0, 6, 1), fun j -> &B[MINDEX2(8, 6, "
          "i, j)] ~> Cell), i := 1, bound_check_start := checked, "
          "bound_check_stop := checked, bound_check_step := checked");
  __ghost(group_ro_focus,
          "items := fun j -> &B[MINDEX2(8, 6, 1, j)] ~> Cell, i := 2, "
          "bound_check_start := checked, bound_check_stop := checked, "
          "bound_check_step := checked");
  x += B[MINDEX2(8, 6, 1, 2)];
  __ghost(group_ro_unfocus,
          "items := fun j -> &B[MINDEX2(8, 6, 1, j)] ~> Cell, i := 2, "
          "bound_check_start := checked, bound_check_stop := checked, "
          "bound_check_step := checked");
  __ghost(group_unfocus,
          "items := fun i -> Group(range(0, 6, 1), fun j -> &B[MINDEX2(8, 6, "
          "i, j)] ~> Cell), i := 1, bound_check_start := checked, "
          "bound_check_stop := checked, bound_check_step := checked");
  MFREE2(8, 6, B);
}
