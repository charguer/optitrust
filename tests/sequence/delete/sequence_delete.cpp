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
  int y = z++;
  // Pas d'utilisation de y
}

void shadowed(int* y) {
  __modifies("y ~> Matrix1(2)");

  int x;
  x = 3;
  x = 4;

  y[0] = x;
  x++;
  y[0] = x;
}

void wrong_target() {
  __pure();

  int r = 8;
}
