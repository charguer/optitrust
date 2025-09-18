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
  b++;
}

void copy_back() {
  __pure();
  int a = 0;
  a++;
  int c = a;
}

void bad_write_after_copy_back() {
  __pure();
  int a = 0;
  int b = a;
  b++;
  a = b;
  b = 3;
}

void bad_read_after_copy_back() {
  __pure();
  int a = 0;
  int b = a;
  b++;
  a = b;
  a = 3;
  int c = b;
}

void mutltiple_copy_backs() {
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
