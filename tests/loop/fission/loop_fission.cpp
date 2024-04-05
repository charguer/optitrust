#include <optitrust.h>

void pure(int n, int m, int o, int p, int q) {
  __pure();

  for (int i = 1; i < n; i++) {
    __strict();

    int a = i;
    int b = i;
    int c = i;
    int d = i;
  }

  for (int i = 1; i < n; i++) {
    __strict();

    for (int j = 1; j < m; j++) {
      __strict();

      int x = i;
      int b = i;
      int c = i;
    }
  }

  for (int u = 0; u < o; u++) {
    __strict();

    for (int i = 1; i < n; i++) {
      __strict();

      for (int j = 0; j < m; j++) {
      __strict();

        for (int v = 0; v < p; v++) {
          __strict();

          int x = i;
          int c = i;
        }
        int e = j;
      }
    }
  }

  for (int i = 1; i < n; i++) {
    __strict();

    for (int j = 1; j < m; j++) {
      __strict();

      for (int k = 1; k < o; k++) {
        __strict();

        int y = i;
        int b = i;
        int c = i;
      }
    }
  }

  for (int u = 0; u < o; u++) {
    __strict();

    int a;
    for (int v = 0; v < p; v++) {
      __strict();

      for (int i = 1; i < n; i++) {
        __strict();

        for (int j = 1; j < n; j++) {
          __strict();

          for (int k = 1; k < n; k++) {
            __strict();

            int y = i;
            int b = i;
            int c = i;
          }
        }
      }
    }
  }

  // return 0;
}

void seq_ro_par_rw(int m, int n, int o, int* t) {
  __modifies("t ~> Matrix3(m, n, o)");

  int x = 0;
  for (int i = 0; i < m; i++) {
    __strict();
    __sreads("&x ~> Cell");
    __xmodifies("for j in 0..n -> for k in 0..o -> &t[MINDEX3(m, n, o, i, j, k)] ~> Cell");

    for (int j = 0; j < n; j++) {
      __strict();
      __sreads("&x ~> Cell");
      __xmodifies("for k in 0..o -> &t[MINDEX3(m, n, o, i, j, k)] ~> Cell");

      for (int k = 0; k < o; k++) {
        __strict();
        __sreads("&x ~> Cell");
        __xmodifies("&t[MINDEX3(m, n, o, i, j, k)] ~> Cell");

        t[MINDEX3(m, n, o, i, j, k)] = x;
        int y = t[MINDEX3(m, n, o, i, j, k)];
      }
    }
  }
}

void ghost_scope(int m, int n) {
  __pure();

  int x = 0;
  for (int i = 0; i < m; i++) {
    __strict();
    __sreads("&x ~> Cell");

    __GHOST_BEGIN(xfg, ro_fork_group, "H := &x ~> Cell, r:= 0..n");
    for (int j = 0; j < n; j++) {
        __strict();
        __xreads("&x ~> Cell");

        int y = x;
        int z = x;
    }
    __GHOST_END(xfg);
  }
}

__GHOST(ensures_pure) {
  __requires("n: int");
  __ensures("Triv(n)");
  __admitted();
}

void requires_pure(int n) {
  __requires("Triv(n)");
}

void ensures_not_ghost(int n) {
  __ensures("Triv(n)");
  __ghost(ensures_pure, "n");
}

void ghost_pure(int m, int n) {
  __pure();

  for (int i = 0; i < m; i++) {
    __strict();
    __xensures("Triv(6)");

    __ghost(ensures_pure, "1");
    __ghost(ensures_pure, "2");
    __ghost(ensures_pure, "3");
    __ghost(ensures_pure, "4");
    ensures_not_ghost(5);
    ensures_not_ghost(6);
    ensures_not_ghost(7);
    requires_pure(1);
    requires_pure(3);

    split:
    __ghost(ensures_pure, "2");
    requires_pure(2);
    requires_pure(3);
    requires_pure(4);
    requires_pure(5);
    requires_pure(6);
  }
}

void edges() {
  for (int i = 0; i < 5; i++) {
    int x = i;
    x++;
  }
}
