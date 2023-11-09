#include "../../../include/optitrust.h"

void dead_code() {
  __pure();

  int z = 0;
  int y = z;
  int x = 3;
  for (int i = 0; i < 2; i++) {
    // TODO: add z RW in contract
    for (int j = 0; j < 3; j++) {
      // TODO: add z uninit in contract

      z = i + j;
    }

    z = i;
    x = z;
  }
  z = x;
}

