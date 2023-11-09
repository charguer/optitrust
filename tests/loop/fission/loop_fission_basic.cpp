#include "../../../include/optitrust.h"

void parallel(int* t, int* u, int n) {
  __modifies(
    "Group(range(1, n, 1), fun i -> &t[i] ~> Cell),"
    "Group(range(1, n, 1), fun i -> &u[i] ~> Cell)");

  for (int i = 1; i < n; i++) {
    __modifies(
      "&t[i] ~> Cell,"
      "&u[i] ~> Cell");
    int a = i;
    t[i] += a;
    int b = i;
    u[i] += b;
    int c = i;
    u[i] += c;
  }

  for (int i = 0; (i < 5); i++) {
    __pure();
    int y = i;
    int z = i;
  }

  for (int i = 0; i < 3; i++) {
    __pure();
    int* const m1 = (int* const) MALLOC1(5, sizeof(int));
    MFREE1(5, m1);
    int* const m2 = (int* const) MALLOC1(5, sizeof(int));
    MFREE1(5, m2);
  }
}

void commute() {
  __pure();

  int x;
  int y;
  for (int i = 0; i < 5; i++) {
    __sequentially_modifies(
      "&x ~> Cell,"
      "&y ~> Cell");
    x++;
    y++;
  }
}

void wrong() {
  __pure();

  int x;
  for (int i = 0; i < 4; i++) {
    __sequentially_modifies("&x ~> Cell");
    x++;
    x++;
  }
}

int testAllInstr(int* t, int* u, int n) {
  __pure();
  for (int i = 0; (i < 5); i++) {
    __pure();
    int a1 = i;
    int a2 = i;
    int a3 = i;
    int a4 = i;
    int a5 = i;
  }
}

int testAllInstr2(int* t, int* u, int n) {
  __pure();
  for (int i = 0; (i < 5); i++) {
    __pure();
    int a1 = i;
    int a2 = i;
    int a3 = i;
    int a4 = i;
    int a5 = i;
  }
}

int testAllInstrContracts(int* t, int* u, int n) {
    __modifies(
    "Group(range(1, n, 1), fun i -> &t[i] ~> Cell),"
    "Group(range(1, n, 1), fun i -> &u[i] ~> Cell)");
  for (int i = 1; i < n; i++) {
    __modifies(
      "&t[i] ~> Cell,"
      "&u[i] ~> Cell");
    int a = i;
    t[i] += a;
    int b = i;
    u[i] += b;
    int c = i;
    u[i] += c; // TODO replace c with b and what is the error message
  }

}
