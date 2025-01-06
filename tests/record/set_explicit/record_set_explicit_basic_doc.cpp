#include "optitrust.h"

typedef struct {
  int x;
  int y;
} vect;

void f() {
  __pure();

  vect a;
  vect b = {0, 0};
  a = b;
}
