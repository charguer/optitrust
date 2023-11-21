#include <optitrust.h>

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
    z = i;
    x = z;
  }
  z = x;
}

void shadowed(int* y) {
  __modifies("y ~> Cell");
  int x;
  x = 4;
  x = 5;
  x++;
  *y = x;
}

void redundant(int* x) {
  __modifies("x ~> Cell");
  *x = 2;
  *x = 3;
  int r = *x;
  int v = 1;
  *x = v;
  v++;
  *x = v;
}

void wrong_target() {
  __pure();
  int r = 8;
}
