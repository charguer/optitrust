#include <optitrust.h>

void f() {
  __pure();
  int x = 0;
  int* const A = (int*)calloc(MSIZE1(10), sizeof(int));
  const __ghost_fn focusA =
      __ghost_begin(ro_matrix1_focus, "matrix := A, i := 0");
  __ghost_end(focusA);
  const __ghost_fn __ghost_pair_1 =
      __ghost_begin(ro_matrix1_focus, "matrix := A, i := 0");
  x += 1;
  x += A[MINDEX1(10, 0)];
  __ghost_end(__ghost_pair_1);
  const __ghost_fn __ghost_pair_2 =
      __ghost_begin(ro_matrix1_focus, "matrix := A, i := 0");
  x += 1;
  __ghost_end(__ghost_pair_2);
  const __ghost_fn focusA2 =
      __ghost_begin(ro_matrix1_focus, "matrix := A, i := 0");
  __ghost_end(focusA2);
  const __ghost_fn __ghost_pair_3 =
      __ghost_begin(ro_matrix1_focus, "matrix := A, i := 0");
  x += 1;
  __ghost_end(__ghost_pair_3);
  free(A);
  int* const B = (int*)calloc(MSIZE2(8, 6), sizeof(int));
  const __ghost_fn focusBi = __ghost_begin(group_focus, "i := 1");
  x += 2;
  const __ghost_fn focusBj = __ghost_begin(group_focus, "i := 2");
  __ghost_end(focusBj);
  __ghost_end(focusBi);
  const __ghost_fn __ghost_pair_5 = __ghost_begin(group_focus, "i := 1");
  const __ghost_fn __ghost_pair_4 = __ghost_begin(group_focus, "i := 2");
  x += 1;
  x += B[MINDEX2(8, 6, 1, 2)];
  __ghost_end(__ghost_pair_4);
  __ghost_end(__ghost_pair_5);
  const __ghost_fn __ghost_pair_7 = __ghost_begin(group_focus, "i := 1");
  const __ghost_fn __ghost_pair_6 = __ghost_begin(group_focus, "i := 2");
  x += 1;
  __ghost_end(__ghost_pair_6);
  x += 2;
  __ghost_end(__ghost_pair_7);
  const __ghost_fn focusRoBi = __ghost_begin(ro_group_focus, "i := 1");
  const __ghost_fn focusRoBj = __ghost_begin(ro_group_focus, "i := 2");
  __ghost_end(focusRoBj);
  __ghost_end(focusRoBi);
  const __ghost_fn __ghost_pair_9 = __ghost_begin(ro_group_focus, "i := 1");
  const __ghost_fn __ghost_pair_8 = __ghost_begin(ro_group_focus, "i := 2");
  x += 1;
  x += B[MINDEX2(8, 6, 1, 2)];
  __ghost_end(__ghost_pair_8);
  __ghost_end(__ghost_pair_9);
  const __ghost_fn __ghost_pair_11 = __ghost_begin(ro_group_focus, "i := 1");
  const __ghost_fn __ghost_pair_10 = __ghost_begin(ro_group_focus, "i := 2");
  x += 1;
  __ghost_end(__ghost_pair_10);
  __ghost_end(__ghost_pair_11);
  free(B);
}
