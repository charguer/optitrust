#include <optitrust.h>

void f() {
  __pure();
  int a = 0;
  int b = 0;
  {
    a += 2;
    int y = b;
    y += 3;
  }
  b++;
}

void resources_not_available() {
  int a = 0;
  { int x = a; }
}
