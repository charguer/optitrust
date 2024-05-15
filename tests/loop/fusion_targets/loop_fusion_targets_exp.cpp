#include <optitrust.h>

void f(int* t, int* u, int* v, int n) {
  __modifies("for i in 0..n -> &t[i] ~> Cell");
  __modifies("for i in 0..n -> &u[i] ~> Cell");
  __modifies("for i in 0..n -> &v[i] ~> Cell");
  float p = 5.f;
  for (int i = 0; i < n; i++) {
    __strict();
    __xmodifies("&t[i] ~> Cell");
    __xmodifies("&u[i] ~> Cell");
    __xmodifies("&v[i] ~> Cell");
    t[i] = i;
    u[i] += i;
    v[i] += i;
  }
  int x = 1;
  for (int k0 = 0; k0 < n; k0++) {
    __strict();
    __smodifies("&p ~> Cell");
    for (int k1 = 0; k1 < n; k1++) {
      __strict();
      __smodifies("&p ~> Cell");
      const int tmp = k0;
      p += tmp;
    }
  }
  int y = 2;
  for (int k0 = 0; k0 < n; k0++) {
    __strict();
    __smodifies("&p ~> Cell");
    for (int k1 = 0; k1 < n; k1++) {
      __strict();
      __smodifies("&p ~> Cell");
      const int tmp = k1;
      p += tmp;
    }
  }
  int a;
  int b;
  int c;
}

void with_deps1(int n) {
  __pure();
  int a = 0;
  int b = 0;
  int x = 0;
  int y = 0;
  int u = 0;
  int v = 0;
  y += b;
  for (int i = 0; i < n; i++) {
    __strict();
    __smodifies("&a ~> Cell");
    __smodifies("&b ~> Cell");
    a++;
    b++;
  }
  x += a;
  u += x;
  v += 1;
}

void with_deps2(int n) {
  __pure();
  int a = 0;
  int b = 0;
  int x = 0;
  int y = 0;
  int u = 0;
  int v = 0;
  v += 1;
  y += b;
  for (int j = 0; j < n; j++) {
    __strict();
    __smodifies("&a ~> Cell");
    __smodifies("&b ~> Cell");
    a++;
    b++;
  }
  x += a;
  u += x;
}
