#include <optitrust.h>

void f() {
  __pure();

  int a = 0;
  int b = 0;
  {
    int x = a;
    x += 2;
    int y = b;
    y += 3;
  }
  b++;
}

void g() {
  __pure();
  int a = 0;
  int b = a;
  b++;
  a = b;
}

void h() {
  __pure();
  int a = 0;
  int b = a;
  b++;
  a = b;
  b++;
}


void resources_not_available() {
  int a = 0;
  {
    int x = a;
  }
}
