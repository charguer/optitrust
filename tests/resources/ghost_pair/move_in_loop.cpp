#include "optitrust.h"
void simple_ghost_in(float *x) {
  __reads("x~>Matrix1(10)");
  __GHOST_BEGIN(focus, ro_matrix1_focus, "i:=1, n :=10, matrix := x");
  for (int i = 0; i < 10; i++) {
    float a = x[MINDEX1(10, 1)];
  }
  __GHOST_END(focus);
}

void delete_sreads(float *x) {
  __reads("x~> Matrix2(2,1)");
  for (int j = 0; j < 10; j++) {
    const __ghost_fn __ghost_pair_1 = __ghost_begin(
        ro_group_focus, "range := 0..2, i := 1, items := fun (l: int) ->  for k in 0..1 ->"
                        "&x[MINDEX2(2,1,l,k)]~> Cell ");
    for (int i = 0; i < 2; i++) {

      __ghost(
          [&]() {
             __requires("#_3: _Fraction");
            __consumes("_RO(#_3,for k in 0..1 -> &x[MINDEX2(2,1,1,k)]~> Cell) ");
            __produces("&x[MINDEX2(2,1,1,0)] ~> Cell");
            __produces("&x[MINDEX2(2,1,1,1)] ~> Cell");
            __admitted();
          },
          "");
      float a = x[MINDEX2(2, 1, 1, 0)] + x[MINDEX2(2, 1, 1, 1)];
      __ghost(
          [&]() {
             __requires("#_3: _Fraction");
            __consumes("&x[MINDEX2(2,1,1,0)] ~> Cell");
            __consumes("&x[MINDEX2(2,1,1,1)] ~> Cell");
            __produces("_RO(#_3,for k in 0..1 -> &x[MINDEX2(2,1,1,k)]~> Cell) ");
            __admitted();
          },
          "");
    }
    __GHOST_END(__ghost_pair_1);
  }
}
// NOT HANDLED YET
// void ghost_in_invariant(float *x)
// {
// __modifies("x~>Matrix1(10)");
// __GHOST_BEGIN(focus,group_focus_subrange, "sub_range := 2..6,big_range := 0..10 ");
// for (int i = 2; i < 6 ; i++){
//   __xmodifies("&x[MINDEX1(10,i)]~> Cell");
//   x[MINDEX1(10,i)] = 3.f;
// }
// __GHOST_END(focus);

// }
