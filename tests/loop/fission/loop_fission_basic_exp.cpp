#include "../../../include/optitrust.h"

int* const t;

int* const u;

const int n;

int main() {
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
