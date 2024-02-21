#include <optitrust.h>

void pure(int n, int m, int o, int p, int q) {
  __pure();

  for (int i = 1; i < n; i++) {
    __pure();

    int a = i;
    int b = i;
    int c = i;
    int d = i;
  }

  for (int i = 1; i < n; i++) {
    __pure();

    for (int j = 1; j < m; j++) {
      __pure();

      int x = i;
      int b = i;
      int c = i;
    }
  }

  for (int u = 0; u < o; u++) {
    __pure();

    for (int i = 1; i < n; i++) {
      __pure();

      for (int j = 0; j < m; j++) {
      __pure();

        for (int v = 0; v < p; v++) {
          __pure();

          int x = i;
          int c = i;
        }
        int e = j;
      }
    }
  }

  for (int i = 1; i < n; i++) {
    __pure();

    for (int j = 1; j < m; j++) {
      __pure();

      for (int k = 1; k < o; k++) {
        __pure();

        int y = i;
        int b = i;
        int c = i;
      }
    }
  }

  for (int u = 0; u < o; u++) {
    __pure();

    int a;
    for (int v = 0; v < p; v++) {
      __pure();

      for (int i = 1; i < n; i++) {
        __pure();

        for (int j = 1; j < n; j++) {
          __pure();

          for (int k = 1; k < n; k++) {
            __pure();

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
    __parallel_reads("&x ~> Cell");
    __modifies("Group(range(0, n, 1), fun j -> Group(range(0, o, 1), fun k -> &t[MINDEX3(m, n, o, i, j, k)] ~> Cell))");

    for (int j = 0; j < n; j++) {
      __parallel_reads("&x ~> Cell");
      __modifies("Group(range(0, o, 1), fun k -> &t[MINDEX3(m, n, o, i, j, k)] ~> Cell)");

      for (int k = 0; k < o; k++) {
        __parallel_reads("&x ~> Cell");
        __modifies("&t[MINDEX3(m, n, o, i, j, k)] ~> Cell");

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
    __parallel_reads("&x ~> Cell");

    __GHOST_BEGIN(xfg, ro_fork_group, "H := &x ~> Cell, r:= range(0, n, 1)");
    for (int j = 0; j < n; j++) {
        __reads("&x ~> Cell");

        int y = x;
        int z = x;
    }
    __GHOST_END(xfg);
  }
}

void edges() {
  for (int i = 0; i < 5; i++) {
    int x = i;
    x++;
  }
}
