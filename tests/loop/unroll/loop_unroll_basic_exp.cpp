#include <optitrust.h>

void simple(int* t) {
  int a = 2;
  t[0] = a;
  t[1] = a;
  t[2] = a;
}

void name_conflict_no_brace() {
  int a = 0;
  int a1 = 1;
}

void name_conflict_with_braces() {
  { int a = 0; }
  { int a = 1; }
}

void start_add(int a) {
  int b = a + 0;
  int b2 = a + 1;
}

void step() {
  int b = 0;
  int b3 = 2;
  int b4 = 4;
  int c = 0;
  int c5 = 3;
}

void iter_contract(int* M) {
  __modifies("M ~> Matrix1(3)");
  __ghost(
      [&]() {
        __consumes("M ~> Matrix1(3)");
        __produces("&M[MINDEX1(3, 0)] ~> Cell");
        __produces("&M[MINDEX1(3, 1)] ~> Cell");
        __produces("&M[MINDEX1(3, 2)] ~> Cell");
        __admitted();
        __with("justif := unroll");
      },
      "");
  M[MINDEX1(3, 0)] = 0;
  M[MINDEX1(3, 1)] = 0;
  M[MINDEX1(3, 2)] = 0;
  __ghost(
      [&]() {
        __consumes("&M[MINDEX1(3, 0)] ~> Cell");
        __consumes("&M[MINDEX1(3, 1)] ~> Cell");
        __consumes("&M[MINDEX1(3, 2)] ~> Cell");
        __produces("M ~> Matrix1(3)");
        __admitted();
        __with("justif := roll");
      },
      "");
}
