#include <optitrust.h>

const int N = 2;

const int L = 4;

const int U = 7;

void f() {
  int s = 0;
  int a = 1;
  int b = a + 2;
  int c = 3;
  int d = c + 4;
  int e = d + 5;
  int a3 = 1;
  int b4 = a3 + 2;
  int c5 = 3;
  int d6 = c5 + 4;
  int e7 = d6 + 5;
  int x = 4;
  int y = 5;
  int x8 = 5;
  int y9 = 6;
  int x10 = 6;
  int y11 = 7;
  s = 0;
  s = 1;
  s = 1;
  s = 2;
}

void iter_contract_ro(int* M) {
  __reads("M ~> Matrix1(3)");
  int acc = 0;
  __ghost(assume, "P := in_range(0, 0..3)");
  __ghost(assume, "P := in_range(1, 0..3)");
  __ghost(assume, "P := in_range(2, 0..3)");
  const __ghost_fn __ghost_pair_1 = __ghost_begin(
      ro_group_focus,
      "i := 0, items := fun (x: int) -> &M[MINDEX1(3, x)] ~> Cell");
  acc += M[MINDEX1(3, 0)];
  __ghost_end(__ghost_pair_1);
  const __ghost_fn __ghost_pair_11 = __ghost_begin(
      ro_group_focus,
      "i := 1, items := fun (x: int) -> &M[MINDEX1(3, x)] ~> Cell");
  acc += M[MINDEX1(3, 1)];
  __ghost_end(__ghost_pair_11);
  const __ghost_fn __ghost_pair_12 = __ghost_begin(
      ro_group_focus,
      "i := 2, items := fun (x: int) -> &M[MINDEX1(3, x)] ~> Cell");
  acc += M[MINDEX1(3, 2)];
  __ghost_end(__ghost_pair_12);
}
