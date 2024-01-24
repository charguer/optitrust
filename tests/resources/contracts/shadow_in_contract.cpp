#include <optitrust.h>

int x = 0;

void f() {
  __requires("P(fun y -> x)");
}
