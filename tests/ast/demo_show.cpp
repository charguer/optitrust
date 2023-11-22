#include "../../include/optitrust.h"

int main() {
  int a, b;
  int x = 3;
  x--;
  for (int i = 0; i < 3; i++) {
    __sequentially_modifies("x ~> Cell");
    x++;
  }
}
