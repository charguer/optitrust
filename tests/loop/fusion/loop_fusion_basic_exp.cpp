#include <optitrust.h>

void parallel(int* t, int* u, int n) {
  __modifies("for i in 1..n -> &t[i] ~> Cell");
  __modifies("for i in 1..n -> &u[i] ~> Cell");
  for (int i = 1; i < n; i++) {
    __strict();
    __xmodifies("&t[i] ~> Cell");
    __xmodifies("&u[i] ~> Cell");
    int a = i;
    t[i] += a;
    int b = i;
    u[i] += b + t[i];
    int c = i;
    u[i] += c;
  }
  for (int i = 0; i < 5; i++) {
    __strict();
    int y = i;
    int z = i;
  }
  for (int i = 0; i < 3; i++) {
    __strict();
    int* const m1 = (int* const)MALLOC1(5, sizeof(int));
    MFREE1(5, m1);
    int* const m2 = (int* const)MALLOC1(5, sizeof(int));
    MFREE1(5, m2);
  }
}

void uninit(int* t, int* u, int n) {
  __writes("for i in 1..n -> &t[i] ~> Cell");
  int x = 0;
  for (int i = 1; i < n; i++) {
    __strict();
    __smodifies("&x ~> Cell");
    __xwrites("&t[i] ~> Cell");
    t[i] = i;
    x += t[i];
  }
}

void commute() {
  __pure();
  int x;
  int y;
  for (int i = 0; i < 5; i++) {
    __strict();
    __smodifies("&x ~> Cell");
    __smodifies("&y ~> Cell");
    x++;
    y++;
  }
  int z = 2;
  for (int j = 0; j < 5; j++) {
    __strict();
    __smodifies("&x ~> Cell");
    __smodifies("&y ~> Cell");
    __sreads("&z ~> Cell");
    __sreads("&z ~> Cell");
    x += z;
    y += z;
  }
  for (int k1 = 0; k1 < 5; k1++) {
    __strict();
    __smodifies("&x ~> Cell");
    __smodifies("&y ~> Cell");
    __smodifies("&z ~> Cell");
    x += 1;
    for (int k2 = 0; k2 < 5; k2++) {
      __strict();
      __smodifies("&y ~> Cell");
      y += 1;
    }
    z += 1;
  }
}

void excl_ros(int* t, int n) {
  __reads("for i in 1..n -> &t[i] ~> Cell");
  int x = 0;
  int y = 0;
  for (int i = 1; i < n; i++) {
    __strict();
    __smodifies("&y ~> Cell");
    __smodifies("&x ~> Cell");
    __xreads("&t[i] ~> Cell");
    __xreads("&t[i] ~> Cell");
    y += t[i];
    x += t[i];
  }
}

void wrong_rw_rw() {
  __pure();
  int x = 0;
  for (int i = 0; i < 4; i++) {
    __strict();
    __smodifies("&x ~> Cell");
    x++;
  }
  for (int i = 0; i < 4; i++) {
    __strict();
    __smodifies("&x ~> Cell");
    x++;
  }
}

void wrong_rw_ro() {
  __pure();
  int x = 0;
  int y = 0;
  for (int i = 0; i < 4; i++) {
    __strict();
    __smodifies("&x ~> Cell");
    x++;
  }
  for (int i = 0; i < 4; i++) {
    __strict();
    __smodifies("&y ~> Cell");
    __sreads("&x ~> Cell");
    y += x;
  }
}

void wrong_ro_rw() {
  __pure();
  int x = 0;
  int y = 0;
  for (int i = 0; i < 4; i++) {
    __strict();
    __smodifies("&y ~> Cell");
    __sreads("&x ~> Cell");
    y += x;
  }
  for (int i = 0; i < 4; i++) {
    __strict();
    __smodifies("&x ~> Cell");
    x++;
  }
}
