#include <optitrust.h>

void parallel(int* t, int* u, int n) {
  __modifies(
    "for i in 1..n -> &t[i] ~> Cell,"
    "for i in 1..n -> &u[i] ~> Cell");

  for (int i = 1; i < n; i++) {
    __strict();
    __xmodifies(
      "&t[i] ~> Cell,"
      "&u[i] ~> Cell");
    int a = i;
    t[i] += a;
    int b = i;
    u[i] += b + t[i];
    int c = i;
    u[i] += c;
  }

  for (int i = 0; (i < 5); i++) {
    __strict();
    int y = i;
    int z = i;
  }

  for (int i = 0; i < 3; i++) {
    __strict();
    int* const m1 = MALLOC1(int, 5);
    free(m1);
    int* const m2 = MALLOC1(int, 5);
    free(m2);
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

  int x = 0;
  int y = 0;
  for (int i = 0; i < 5; i++) {
    __strict();
    __smodifies(
      "&x ~> Cell,"
      "&y ~> Cell");
    x++;
    y++;
  }

  int z = 2;
  for (int j = 0; j < 5; j++) {
    __strict();
    __sreads("&z ~> Cell");
    __smodifies(
      "&x ~> Cell,"
      "&y ~> Cell");
    x += z;
    y += z;
  }

  for (int k1 = 0; k1 < 5; k1++) {
    __strict();
    __smodifies(
      "&x ~> Cell,"
      "&y ~> Cell,"
      "&z ~> Cell");

    x += 1;
    for (int k2 = 0; k2 < 5; k2++) {
      __strict();
      __smodifies("&y ~> Cell");
      y += 1;
    }
    z += 1;
  }
}

void wrong_rw_rw() {
  __pure();

  int x = 0;
  for (int i = 0; i < 4; i++) {
    __strict();
    __smodifies("&x ~> Cell");
    x++;
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
    __smodifies("&y ~> Cell");
    x++;
    y += x;
  }
}

void wrong_ro_rw() {
  __pure();

  int x = 0;
  int y = 0;
  for (int i = 0; i < 4; i++) {
    __strict();
    __smodifies("&x ~> Cell");
    __smodifies("&y ~> Cell");
    y += x;
    x++;
  }
}

void empty() {
  __pure();

  for (int i = 0; i < 1024; ++i) {
    __strict();
    // empty
  }
}

void testAllInstr(int* t, int* u, int n) {
  __pure();
  for (int i = 0; (i < 5); i++) {
    __strict();
    int a1 = i;
    int a2 = i;
    int a3 = i;
    int a4 = i;
    int a5 = i;
  }
}

void testAllInstr2(int* t, int* u, int n) {
  __pure();
  for (int i = 0; (i < 5); i++) {
    __strict();
    int a1 = i;
    int a2 = i;
    int a3 = i;
    int a4 = i;
    int a5 = i;
  }
}

void testAllInstrContracts(int* t, int* u, int n) {
  __modifies(
    "for i in 1..n -> &t[i] ~> Cell,"
    "for i in 1..n -> &u[i] ~> Cell");
  for (int i = 1; i < n; i++) {
    __strict();
    __xmodifies(
      "&t[i] ~> Cell,"
      "&u[i] ~> Cell");
    int a = i;
    t[i] += a;
    int b = i;
    u[i] += b;
    int c = i;
    u[i] += c; // TODO replace c with b and what is the error message
  }

}

void ghosts() {
  __pure();

  int x = 0;
  __GHOST_BEGIN(fork_out, ro_fork_group, "H := &x ~> Cell, r := 0..5");
  for (int i = 0; i < 5; i++) {
    __strict();
    __xreads("&x ~> Cell");
    __ghost(ro_fork_group, "H := &x ~> Cell, r := 0..5");
    for (int k = 0; k < 5; k++) {
      __strict();
      __sreads("for j in 0..5 -> &x ~> Cell");
      for (int j = 0; j < 5; j++) {
        __strict();
        __xreads("&x ~> Cell");
        x + 1;
      }
    }
    __ghost(ro_join_group, "H := &x ~> Cell, r := 0..5");
  }
  __GHOST_END(fork_out);
}


__GHOST(ensures_pure) {
  __requires("n: int");
  __ensures("n = n");
  __admitted();
}

void requires_pure(int n) {
  __requires("n = n");
}

void ghost_pure_nondep(int m, int n) {
  __pure();

  for (int i = 0; i < m; i++) {
    __strict();
    __xensures("2 = 2");

    __ghost(ensures_pure, "1");
    requires_pure(1);

    split:
    __ghost(ensures_pure, "2");
    requires_pure(1);
    requires_pure(2);
  }
}

void ghost_pure_dep(int m, int n) {
  __pure();

  for (int i = 0; i < m; i++) {
    __strict();

    __ghost(ensures_pure, "i");
    requires_pure(i);

    split:
    requires_pure(i);
  }
}
