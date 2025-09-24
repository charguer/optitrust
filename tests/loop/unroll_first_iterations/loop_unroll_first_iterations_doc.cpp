#include <optitrust.h>

void f() {
  __pure();

  int x = 0;
  for (int i = 0; i < 10; i++) {
    x += i;
  }
}
