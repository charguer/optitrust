#include <optitrust.h>

void simple(int* t){
  int a = 2;
  for (int i = 0; i < 3; i++) {
    t[i] = a;
  }
}

void name_conflict_no_brace() {
  for (int i = 0; i < 2; i++) {
    int a = i;
  }
}

void name_conflict_with_braces() {
  for (int i = 0; i < 2; i++) {
    int a = i;
  }
}

void start_add(int a) {
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
