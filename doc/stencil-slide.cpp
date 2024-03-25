#include <optitrust.h>

void stencil(int* a, int* b) {
  __writes("a ~> Matrix1(8)");
  __writes("b ~> Matrix1(6)");

  for (int i = 0; i < 6; i++) {
    if (i == 0) {
      __modifies("_Uninit(for k in 0..8 -> a[MINDEX1(8, k)] ~> Cell)");
    } else {
      __modifies("for k in 0..i+2 -> a[MINDEX1(8, k)] ~> Cell");
      __modifies("_Uninit(for k in i+2..8 -> a[MINDEX1(8, k)] ~> Cell)");
    }

    if (i == 0) {
      // Uninit(0..8)
      __ghost(group_split_uninit, "start := 0, stop := 8, split := i+3, items := fun k -> a[MINDEX1(8, k)] ~> Cell");
      // Uninit(i..i+3) * Uninit(i+3..8)
      // TODO: 0..i empty group?
    } else {
      // 0..i+2 * Uninit(i+2..8)
      __ghost(group_split, "stop := i+2, split := i, items := fun k -> a[MINDEX1(8, k)] ~> Cell");
      // 0..i * i..i+2 * Uninit(i+2..8)
      __ghost(group_split_uninit, "start := i+2, split := i+3, items := fun k -> a[MINDEX1(8, k)] ~> Cell");
      // 0..i * i..i+2 * Uninit(i+2..i+3) * Uninit(i+3..8)
      __ghost(forget_init, "for k in i..i+2 -> a[MINDEX1(8, k)] ~> Cell");
      // 0..i * Uninit(i..i+2) * Uninit(i+2..i+3) * Uninit(i+3..8)
      __ghost(group_join_uninit, "start := i, stop := i+3, split := i+2, items := fun k -> a[MINDEX1(8, k)] ~> Cell");
      // 0..i * Uninit(i..i+3) * Uninit(i+3..8)
    }
    // TODO: __ghost(group_shift_uninit)
    for (int j = 0; j < 3; j++) {
      __writes("a[MINDEX1(8, i+j)] ~> Cell");
      a[MINDEX1(8, i+j)] = i+j;
    }
    // TODO: __ghost(group_unshift)
    // 0..i * i..i+3 * Uninit(i+3..8)
    __ghost(group_join, "start := 0, stop := i+3, split := i, items := fun k -> a[MINDEX1(8, k)] ~> Cell");
    // 0..i+3 * Uninit(i+3..8)
  }
  for (int i = 0; i < 6; i++) {
    __writes("b[MINDEX1(6, i)] ~> Cell");
    __parallel_reads("a ~> Matrix1(8)");
    // TODO: in_range_extend, in_range_shift_extend, matrix2_ro_focus scopes
    b[MINDEX1(6, i)] = a[MINDEX1(8, i)] + a[MINDEX1(8, i+1)] + a[MINDEX1(8, i+2)];
  }
}
