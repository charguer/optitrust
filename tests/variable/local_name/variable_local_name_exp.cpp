#include <optitrust.h>

void ok1() {
  __pure();
  int a = 0;
  for (int j = 0; j < 10; j++) {
    __strict();
    __smodifies("&a ~> Cell");
    int x = a;
    for (int i = 0; i < j; i++) {
      __strict();
      __smodifies("&x ~> Cell");
      x++;
    }
    a = x;
  }
}

void ok2() {
  __pure();
  int a = 0;
  int x = a;
l : { x++; }
  a = x;
  int y = 0;
}

void ko1() {
  __pure();
  int a = 0;
  int& b = a;
  for (int j = 0; j < 10; j++) {
    __strict();
    __smodifies("&a ~> Cell");
    for (int i = 0; i < j; i++) {
      __strict();
      __smodifies("&a ~> Cell");
      a++;
      b++;
    }
  }
  int y = 0;
}

void ko2() {
  __pure();
  int a = 0;
  int& b = a;
l: {
  a++;
  b++;
}
  int y = 0;
}

void ko_scope() {
  __pure();
  int x = 0;
  int a = 0;
l: { a++; }
}

void ok3() {
  __pure();
  int a = 0;
  for (int i = 0; i < 10; i++) {
    __strict();
    __smodifies("&a ~> Cell");
    int x = a;
    x++;
    a = x;
  }
}

void ok4() {
  __pure();
  int a = 0;
  int x = a;
  int b = 0;
  x++;
  a = x;
  b++;
}
