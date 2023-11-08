#include "../../../include/optitrust.h"

typedef struct {
  int x;
  int y;
} vect;

void dead_code() {
  __pure();

  int a = 5;
  a++; // __post_inc(a);

  /* TODO:
  vect v = {0,0};
  vect u;
  u.x = 0;
  vect w;
  u.y = 0;
  */

  int z = 0;
  z++;
  int y = z;
  z++;

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

void shadowed(int* y) {
  __modifies("y ~> Cell");

  int x;
  x = 3;
  x = 4;
  x = 5;

  *y = x;
  x++;
  *y = x;
}

void wrong_target() {
  __pure();

  int r = 8;
}
