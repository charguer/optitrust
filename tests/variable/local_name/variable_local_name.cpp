#include <optitrust.h>

typedef int T;

void ok1() {
  __pure();

  T a;
  for (int j = 0; j < 10; j++) {
    __strict();
    __smodifies("&a ~> Cell");
    for (int i = 0; i < j; i++) {
      __strict();
      __smodifies("&a ~> Cell");
      a++;
    }
  }
}

void ok2() {
  __pure();

  T a;
  l: {
    a++;
  }

  int y = 0;
}

void ko1() {
  __pure();

  T a;
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

  T a;
  int& b = a;
  l: {
    a++;
    b++;
  }

  int y = 0;
}

void ko_scope() {
  __pure();
  T x;
  T a;
  l: {
    a++;
  }
}
