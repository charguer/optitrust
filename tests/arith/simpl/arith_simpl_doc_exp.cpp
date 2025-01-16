#include <optitrust.h>

void f() {
  __pure();
  int a = 5;
  int b = 7 * a;
  int c = a * 3 * a / a;
  int d = a * c + b * c;
}
