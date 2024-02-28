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
  int a1 = 1;
  int b2 = a1 + 2;
  int c3 = 3;
  int d4 = c3 + 4;
  int e5 = d4 + 5;
  int x = 4;
  int y = 5;
  int x6 = 5;
  int y7 = 6;
  int x8 = 6;
  int y9 = 7;
  s = 0;
  s = 1;
  s = 1;
  s = 2;
}

void iter_contract_ro(int* M) {
  __reads("M ~> Matrix1(3)");
  int acc = 0;
  const __ghost_fn __ghost_pair_1 = __ghost_begin(
      group_ro_focus, "i := 0, items := fun x -> &M[MINDEX1(3, x)] ~> Cell");
  acc += M[MINDEX1(3, 0)];
  __ghost_end(__ghost_pair_1);
  const __ghost_fn __ghost_pair_110 = __ghost_begin(
      group_ro_focus, "i := 1, items := fun x -> &M[MINDEX1(3, x)] ~> Cell");
  acc += M[MINDEX1(3, 1)];
  __ghost_end(__ghost_pair_110);
  const __ghost_fn __ghost_pair_111 = __ghost_begin(
      group_ro_focus, "i := 2, items := fun x -> &M[MINDEX1(3, x)] ~> Cell");
  acc += M[MINDEX1(3, 2)];
  __ghost_end(__ghost_pair_111);
}
