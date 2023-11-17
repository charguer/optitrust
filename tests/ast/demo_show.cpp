#include "../../include/optitrust.h"

int main() {
  int x = 3;
  for (int i = 0; i < 3; i++) {
    __sequentially_modifies("x ~> Cell");
    x++;
  }
}
