#include <optitrust.h>

void f(int* t, int* u, int* v, int n) {
  __modifies("for i in 0..n -> &t[i] ~> Cell");
  __modifies("for i in 0..n -> &u[i] ~> Cell");
  __modifies("for i in 0..n -> &v[i] ~> Cell");

  float p = 5.0;
  for (int i = 0; i < n; i++) {
    __xmodifies("&t[i] ~> Cell");
    t[i] = i;
  }
  int x = 1;
  for (int i = 0; i < n; i++) {
    __xmodifies("&u[i] ~> Cell");
    u[i] += i;
  }
  for (int k0 = 0; k0 < n; k0++) {
    for (int k1 = 0; k1 < n; k1++) {
      const int tmp = k0;
      p += tmp;
    }
  }
  int y = 2;
  for (int k0 = 0; k0 < n; k0++) {
    for (int k1 = 0; k1 < n; k1++) {
      const int tmp = k1;
      p += tmp;
    }
  }
  for (int j = 0; j < n; j++) {
    __xmodifies("&v[j] ~> Cell");
    v[j] += j;
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

  for (int i = 0; i < n; i++) {
    __smodifies("&a ~> Cell");
    a++;
  }
  x += a;
  u += x;
  v += 1;
  y += b;
  for (int j = 0; j < n; j++) {
    __smodifies("&b ~> Cell");
    b++;
  }
}

void with_deps2(int n) {
  __pure();

  int a = 0;
  int b = 0;
  int x = 0;
  int y = 0;
  int u = 0;
  int v = 0;

  for (int i = 0; i < n; i++) {
    __smodifies("&a ~> Cell");
    a++;
  }
  x += a;
  u += x;
  v += 1;
  y += b;
  for (int j = 0; j < n; j++) {
    __smodifies("&b ~> Cell");
    b++;
  }
}
