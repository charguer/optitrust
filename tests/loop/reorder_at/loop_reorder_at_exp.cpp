#include "../../../include/optitrust.h"

void f1() {
  __pure();
  int r = 0;
  for (int a = 0; a < 4; a++) {
    for (int b = 0; b < 4; b++) {
      r += 1;
    }
    for (int c = 0; c < 4; c++) {
      for (int b = 0; b < 4; b++) {
        r++;
      }
    }
    for (int b = 0; b < 4; b++) {
      r += 2;
    }
  }
}
