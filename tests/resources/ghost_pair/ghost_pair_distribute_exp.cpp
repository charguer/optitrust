#include "../../../include/optitrust.h"

int main() {
  __pure();
  int x = 0;
  float* const A = (float* const)MALLOC1(10, sizeof(float));
  const __ghost_fn focusA = __ghost_begin(matrix1_ro_focus, "M := A, i := 0");
  __ghost_end(focusA);
  const __ghost_fn __ghost_pair_0 =
      __ghost_begin(matrix1_ro_focus, "M := A, i := 0");
  x += 1;
  x += A[MINDEX1(10, 0)];
  __ghost_end(__ghost_pair_0);
  const __ghost_fn __ghost_pair_1 =
      __ghost_begin(matrix1_ro_focus, "M := A, i := 0");
  x += 1;
  __ghost_end(__ghost_pair_1);
  const __ghost_fn focusA2 = __ghost_begin(matrix1_ro_focus, "M := A, i := 0");
  __ghost_end(focusA2);
  const __ghost_fn __ghost_pair_2 =
      __ghost_begin(matrix1_ro_focus, "M := A, i := 0");
  x += 1;
  __ghost_end(__ghost_pair_2);
  MFREE1(10, A);
  float* const B = (float* const)MALLOC2(8, 6, sizeof(float));
  const __ghost_fn focusBi =
      __ghost_begin(group_focus,
                    "i := 1, bound_check_start := checked, bound_check_stop := "
                    "checked, bound_check_step := checked");
  x += 2;
  const __ghost_fn focusBj =
      __ghost_begin(group_focus,
                    "i := 2, bound_check_start := checked, bound_check_stop := "
                    "checked, bound_check_step := checked");
  __ghost_end(focusBj);
  __ghost_end(focusBi);
  const __ghost_fn __ghost_pair_4 =
      __ghost_begin(group_focus,
                    "i := 1, bound_check_start := checked, bound_check_stop := "
                    "checked, bound_check_step := checked");
  const __ghost_fn __ghost_pair_3 =
      __ghost_begin(group_focus,
                    "i := 2, bound_check_start := checked, bound_check_stop := "
                    "checked, bound_check_step := checked");
  x += 1;
  x += B[MINDEX2(8, 6, 1, 2)];
  __ghost_end(__ghost_pair_3);
  __ghost_end(__ghost_pair_4);
  const __ghost_fn __ghost_pair_6 =
      __ghost_begin(group_focus,
                    "i := 1, bound_check_start := checked, bound_check_stop := "
                    "checked, bound_check_step := checked");
  const __ghost_fn __ghost_pair_5 =
      __ghost_begin(group_focus,
                    "i := 2, bound_check_start := checked, bound_check_stop := "
                    "checked, bound_check_step := checked");
  x += 1;
  __ghost_end(__ghost_pair_5);
  x += 2;
  __ghost_end(__ghost_pair_6);
  const __ghost_fn focusRoBi =
      __ghost_begin(group_ro_focus,
                    "i := 1, bound_check_start := checked, bound_check_stop := "
                    "checked, bound_check_step := checked");
  const __ghost_fn focusRoBj =
      __ghost_begin(group_ro_focus,
                    "i := 2, bound_check_start := checked, bound_check_stop := "
                    "checked, bound_check_step := checked");
  __ghost_end(focusRoBj);
  __ghost_end(focusRoBi);
  const __ghost_fn __ghost_pair_8 =
      __ghost_begin(group_ro_focus,
                    "i := 1, bound_check_start := checked, bound_check_stop := "
                    "checked, bound_check_step := checked");
  const __ghost_fn __ghost_pair_7 =
      __ghost_begin(group_ro_focus,
                    "i := 2, bound_check_start := checked, bound_check_stop := "
                    "checked, bound_check_step := checked");
  x += 1;
  x += B[MINDEX2(8, 6, 1, 2)];
  __ghost_end(__ghost_pair_7);
  __ghost_end(__ghost_pair_8);
  const __ghost_fn __ghost_pair_10 =
      __ghost_begin(group_ro_focus,
                    "i := 1, bound_check_start := checked, bound_check_stop := "
                    "checked, bound_check_step := checked");
  const __ghost_fn __ghost_pair_9 =
      __ghost_begin(group_ro_focus,
                    "i := 2, bound_check_start := checked, bound_check_stop := "
                    "checked, bound_check_step := checked");
  x += 1;
  __ghost_end(__ghost_pair_9);
  __ghost_end(__ghost_pair_10);
  MFREE2(8, 6, B);
}