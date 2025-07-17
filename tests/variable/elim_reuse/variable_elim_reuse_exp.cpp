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

void g() {
  __pure();
  int a = 0;
  a++;
  int c = a;
}

void h() {
  __pure();
  int a = 0;
  int b = a;
  b++;
  a = b;
  b = 3;
}

void i() {
  __pure();
  int a = 0;
  int b = a;
  b++;
  a = b;
  a = 3;
  int c = b;
}

void j() {
  __pure();
  int a = 0;
  int b = a;
  b++;
  a = b;
  a = b;
}

void resources_not_available() {
  int a = 0;
  { int x = a; }
}
