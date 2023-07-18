#include "../../include/optitrust.h"

void h() {
  __pure();
  int x = 0;
  int y = 0;
  int z = 0;
  for (int i = 0; i < 3; ++i) {
    __sequentially_modifies("z ~> Cell;");
    __sequentially_reads("y ~> Cell;");
    z = y;
  }
}
