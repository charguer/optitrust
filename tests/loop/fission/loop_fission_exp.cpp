#include <optitrust.h>

void pure(int n, int m, int o, int p, int q) {
  __pure();
  for (int i = 1; i < n; i++) {
    __strict();
    int a = i;
  }
  for (int i = 1; i < n; i++) {
    __strict();
    int b = i;
  }
  for (int i = 1; i < n; i++) {
    __strict();
    int c = i;
  }
  for (int i = 1; i < n; i++) {
    __strict();
    int d = i;
  }
  for (int i = 1; i < n; i++) {
    __strict();
    for (int j = 1; j < m; j++) {
      __strict();
      int x = i;
    }
  }
  for (int i = 1; i < n; i++) {
    __strict();
    for (int j = 1; j < m; j++) {
      __strict();
      int b = i;
    }
  }
  for (int i = 1; i < n; i++) {
    __strict();
    for (int j = 1; j < m; j++) {
      __strict();
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
      }
    }
    for (int i = 1; i < n; i++) {
      __strict();
      for (int j = 0; j < m; j++) {
        __strict();
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
      }
    }
  }
  for (int i = 1; i < n; i++) {
    __strict();
    for (int j = 1; j < m; j++) {
      __strict();
      for (int k = 1; k < o; k++) {
        __strict();
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
          }
        }
      }
      for (int i = 1; i < n; i++) {
        __strict();
        for (int j = 1; j < n; j++) {
          __strict();
          for (int k = 1; k < n; k++) {
            __strict();
            int c = i;
          }
        }
      }
    }
  }
}

void seq_ro_par_rw(int m, int n, int o, int* t) {
  __modifies("t ~> Matrix3(m, n, o)");
  int x = 0;
  for (int i = 0; i < m; i++) {
    __strict();
    __parallel_reads("&x ~> Cell");
    __writes(
        "for j in 0..n -> for k in 0..o -> &t[MINDEX3(m, n, o, i, j, k)] ~> "
        "Cell");
    for (int j = 0; j < n; j++) {
      __strict();
      __parallel_reads("&x ~> Cell");
      __writes("for k in 0..o -> &t[MINDEX3(m, n, o, i, j, k)] ~> Cell");
      for (int k = 0; k < o; k++) {
        __strict();
        __parallel_reads("&x ~> Cell");
        __writes("&t[MINDEX3(m, n, o, i, j, k)] ~> Cell");
        t[MINDEX3(m, n, o, i, j, k)] = x;
      }
    }
  }
  for (int i = 0; i < m; i++) {
    __strict();
    __reads(
        "for j in 0..n -> for k in 0..o -> &t[MINDEX3(m, n, o, i, j, k)] ~> "
        "Cell");
    for (int j = 0; j < n; j++) {
      __strict();
      __reads("for k in 0..o -> &t[MINDEX3(m, n, o, i, j, k)] ~> Cell");
      for (int k = 0; k < o; k++) {
        __strict();
        __reads("&t[MINDEX3(m, n, o, i, j, k)] ~> Cell");
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
    __parallel_reads("&x ~> Cell");
    const __ghost_fn xfg =
        __ghost_begin(ro_fork_group, "H := &x ~> Cell, r := 0..n");
    for (int j = 0; j < n; j++) {
      __strict();
      __reads("&x ~> Cell");
      int y = x;
    }
    __ghost_end(xfg);
  }
  for (int i = 0; i < m; i++) {
    __strict();
    __parallel_reads("&x ~> Cell");
    const __ghost_fn __ghost_pair_1 =
        __ghost_begin(ro_fork_group, "H := &x ~> Cell, r := 0..n");
    for (int j = 0; j < n; j++) {
      __strict();
      __reads("&x ~> Cell");
      int z = x;
    }
    __ghost_end(__ghost_pair_1);
  }
}

void edges() {
  for (int i = 0; i < 5; i++) {
    int x = i;
    x++;
  }
}
