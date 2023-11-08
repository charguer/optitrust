#include "../../../include/optitrust.h"

int test(int* t, int* u, int n) {
  __modifies("Group(range(1, n, 1), fun i -> &t[i] ~> Cell)");
  __modifies("Group(range(1, n, 1), fun i -> &u[i] ~> Cell)");
  for (int i = 1; i < n; i++) {
    __modifies("&t[i] ~> Cell");
    __modifies("&u[i] ~> Cell");
    int a = i;
    t[i] += a;
  }
  for (int i = 1; i < n; i++) {
    __modifies("&t[i] ~> Cell");
    __modifies("&u[i] ~> Cell");
    int b = i;
    u[i] += b;
    int c = i;
    u[i] += c;
  }
  for (int i = 0; i < 5; i++) {
    __pure();
    int y = i;
  }
  for (int i = 0; i < 5; i++) {
    __pure();
    int z = i;
  }
  for (int i = 0; i < 3; i++) {
    __pure();
    int* const m1 = (int* const)MALLOC1(5, sizeof(int));
    MFREE1(5, m1);
    int* const m2 = (int* const)MALLOC1(5, sizeof(int));
    MFREE1(5, m2);
  }
}

int testAllInstr(int* t, int* u, int n) {
  __pure();
  for (int i = 0; i < 5; i++) {
    __pure();
    int a1 = i;
  }
  for (int i = 0; i < 5; i++) {
    __pure();
    int a2 = i;
  }
  for (int i = 0; i < 5; i++) {
    __pure();
    int a3 = i;
  }
  for (int i = 0; i < 5; i++) {
    __pure();
    int a4 = i;
  }
  for (int i = 0; i < 5; i++) {
    __pure();
    int a5 = i;
  }
}

int testAllInstr2(int* t, int* u, int n) {
  __pure();
  for (int i = 0; i < 5; i++) {
    __pure();
    int a1 = i;
    int a2 = i;
  }
  for (int i = 0; i < 5; i++) {
    __pure();
    int a3 = i;
  }
  for (int i = 0; i < 5; i++) {
    __pure();
    int a4 = i;
    int a5 = i;
  }
}

int testAllInstrContracts(int* t, int* u, int n) {
  __modifies("Group(range(1, n, 1), fun i -> &t[i] ~> Cell)");
  __modifies("Group(range(1, n, 1), fun i -> &u[i] ~> Cell)");
  for (int i = 1; i < n; i++) {
    __modifies("&t[i] ~> Cell");
    __modifies("&u[i] ~> Cell");
    int a = i;
    t[i] += a;
  }
  for (int i = 1; i < n; i++) {
    __modifies("&t[i] ~> Cell");
    __modifies("&u[i] ~> Cell");
    int b = i;
    u[i] += b;
  }
  for (int i = 1; i < n; i++) {
    __modifies("&u[i] ~> Cell");
    __modifies("&t[i] ~> Cell");
    int c = i;
    u[i] += c;
  }
}
