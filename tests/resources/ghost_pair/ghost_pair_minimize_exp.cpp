#include <optitrust.h>

int main() {
  __pure();
  int x = 0;
  float* const A = (float* const)CALLOC1(10, sizeof(float));
  x += 1;
  const __ghost_fn focusA = __ghost_begin(matrix1_ro_focus, "M := A, i := 0");
  x += A[MINDEX1(10, 0)];
  __ghost_end(focusA);
  x += 1;
  x += 1;
  MFREE1(10, A);
  float* const B = (float* const)CALLOC2(8, 6, sizeof(float));
  x += 2;
  x += 1;
  const __ghost_fn focusBi =
      __ghost_begin(group_focus,
                    "i := 1, bound_check_start := checked, bound_check_stop := "
                    "checked, bound_check_step := checked");
  const __ghost_fn focusBj =
      __ghost_begin(group_focus,
                    "i := 2, bound_check_start := checked, bound_check_stop := "
                    "checked, bound_check_step := checked");
  x += B[MINDEX2(8, 6, 1, 2)];
  __ghost_end(focusBj);
  __ghost_end(focusBi);
  x += 1;
  x += 2;
  x += 1;
  const __ghost_fn focusRoBi =
      __ghost_begin(group_ro_focus,
                    "i := 1, bound_check_start := checked, bound_check_stop := "
                    "checked, bound_check_step := checked");
  const __ghost_fn focusRoBj =
      __ghost_begin(group_ro_focus,
                    "i := 2, bound_check_start := checked, bound_check_stop := "
                    "checked, bound_check_step := checked");
  x += B[MINDEX2(8, 6, 1, 2)];
  __ghost_end(focusRoBj);
  __ghost_end(focusRoBi);
  x += 1;
  MFREE2(8, 6, B);
}
