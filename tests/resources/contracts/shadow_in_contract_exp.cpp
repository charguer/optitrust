#include <optitrust.h>

int y = 0;

void f() {
  __requires("P(fun y1 -> y)");
}
