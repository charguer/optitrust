#include <optitrust.h>

void f() {
  __pure();
  int x = 2;
  int y = 8;
  int c = 9;
  if (x > 0) {
    x = 5;
  } else {
    x = 5;
  }
  int a = 3;
  if (x > 0) {
    int b = 4;
    c = 5;
  } else {
    int b = 4;
    c = 5;
  }
}

void g() {
  __pure();
  int x = 2;
  int y = 8;
  int c = 9;
  if (x > 0) {
    x = 5;
    int a = 3;
  } else {
    x = 5;
    int a = 3;
  }
  int b = 4;
  c = 5;
}
