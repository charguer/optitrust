#include "../../../include/optitrust.h"

typedef struct {
  int x;
  int y;
} vect;

void dead_code() {
  __pure();
  int z = 0;
  z++;
  z++;
  int x = 3;
  for (int i = 0; i < 2; i++) {
    for (int j = 0; j < 3; j++) {
    }
    z = i;
    x = z;
  }
}

void shadowed(int* y) {
  __modifies("y ~> Cell");
  int x;
  x = 4;
  x++;
  *y = x;
}

void wrong_target() {
  __pure();
  int r = 8;
}
