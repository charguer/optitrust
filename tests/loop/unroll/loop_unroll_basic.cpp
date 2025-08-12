#include <optitrust.h>

void simple(int* t){
  __writes("for i in 0..3 -> &t[i] ~> Cell");

  int a = 2;
  for (int i = 0; i < 3; i++) {
    __xwrites("&t[i] ~> Cell");
    t[i] = a;
  }
}

void name_conflict_no_brace() {
  __pure();

  for (int i = 0; i < 2; i++) {
    int a = i;
  }
}

void name_conflict_with_braces() {
  __pure();

  for (int i = 0; i < 2; i++) {
    int a = i;
  }
}

void start_add(int a) {
  __pure();

  for (int i = a; i < a + 2; i++) {
    int b = i;
  }
}

void step() {
  for (int i = 0; i < 5; i += 2) {
    int b = i;
  }

  for (int i = 0; i < 6; i += 3) {
    int c = i;
  }
}

void iter_contract(int* M) {
  __modifies("M ~> Matrix1(3)");

  for (int i = 0; i < 3; i++) {
    __strict();
    __xmodifies("&M[MINDEX1(3,i)] ~> Cell");
    M[MINDEX1(3,i)] = 0;
  }
}

void focus_in_range(int* M, int s) {
  __reads("M ~> Matrix1(3)");
  __requires("s >= 0");

  int a = 0;
  for (int i = 0; i < 3; i++) {
    __ghost(in_range_bounds, "i, 0, 3");
    __GHOST_BEGIN(focus, ro_matrix1_focus, "M, i");
    a += M[MINDEX1(3, i)];
    __GHOST_END(focus);
  }

  int b = 0;
  for (int i = s; i < s+3; i++) {
    __ghost(in_range_bounds, "i, s, s+3");
    /* __ghost(in_range_shift, "i, -s, s, s+3");
    __GHOST_BEGIN(focus, ro_matrix1_focus, "M, i - s");
    b += M[MINDEX1(3, i - s)];
    __GHOST_END(focus); */
  }
}
