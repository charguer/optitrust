#include <optitrust.h>

void f() {
  __pure();
  int a = 5;
  int b = 7 * a;
  int c = 3 * a;
  int d = a * c + b * c;
}
