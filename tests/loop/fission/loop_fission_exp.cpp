#include <optitrust.h>



  void pure ( int n, int m, int o, int p, int q )  {
__pure();
  for ( int i = 1; i < n; i++ ) {
__pure();
    int a = i;
  }
  for ( int i = 1; i < n; i++ ) {
__pure();
    int b = i;
  }
  for ( int i = 1; i < n; i++ ) {
__pure();
    int c = i;
  }
  for ( int i = 1; i < n; i++ ) {
__pure();
    int d = i;
  }
  for ( int i = 1; i < n; i++ ) {
__pure();
    for ( int j = 1; j < m; j++ ) {
__pure();
      int x = i;
    }
  }
  for ( int i = 1; i < n; i++ ) {
__pure();
    for ( int j = 1; j < m; j++ ) {
__pure();
      int b = i;
    }
  }
  for ( int i = 1; i < n; i++ ) {
__pure();
    for ( int j = 1; j < m; j++ ) {
__pure();
      int c = i;
    }
  }
  for ( int u = 0; u < o; u++ ) {
__pure();
    for ( int i = 1; i < n; i++ ) {
__pure();
      for ( int j = 0; j < m; j++ ) {
__pure();
        for ( int v = 0; v < p; v++ ) {
__pure();
          int x = i;
          int c = i;
        }
      }
    }
    for ( int i = 1; i < n; i++ ) {
__pure();
      for ( int j = 0; j < m; j++ ) {
__pure();
        int e = j;
      }
    }
  }
  for ( int i = 1; i < n; i++ ) {
__pure();
    for ( int j = 1; j < m; j++ ) {
__pure();
      for ( int k = 1; k < o; k++ ) {
__pure();
        int y = i;
        int b = i;
      }
    }
  }
  for ( int i = 1; i < n; i++ ) {
__pure();
    for ( int j = 1; j < m; j++ ) {
__pure();
      for ( int k = 1; k < o; k++ ) {
__pure();
        int c = i;
      }
    }
  }
  for ( int u = 0; u < o; u++ ) {
__pure();
    int a;
    for ( int v = 0; v < p; v++ ) {
__pure();
      for ( int i = 1; i < n; i++ ) {
__pure();
        for ( int j = 1; j < n; j++ ) {
__pure();
          for ( int k = 1; k < n; k++ ) {
__pure();
            int y = i;
            int b = i;
          }
        }
      }
      for ( int i = 1; i < n; i++ ) {
__pure();
        for ( int j = 1; j < n; j++ ) {
__pure();
          for ( int k = 1; k < n; k++ ) {
__pure();
            int c = i;
          }
        }
      }
    }
  }
  }

  void seq_ro_par_rw ( int m, int n, int o, int* t )  {
  __modifies("t ~> Matrix3(m, n, o)");
  int x = 0;
  for ( int i = 0; i < m; i++ ) {
    __sequentially_reads("&x ~> Cell");
    __consumes("_Uninit(Group(range(0, n, 1), fun j -> Group(range(0, o, 1), fun k -> &t[MINDEX3(m, n, o, i, j, k)] ~> Cell)))");
    __produces("Group(range(0, n, 1), fun j -> Group(range(0, o, 1), fun k -> &t[MINDEX3(m, n, o, i, j, k)] ~> Cell))");
    for ( int j = 0; j < n; j++ ) {
      __sequentially_reads("&x ~> Cell");
      __consumes("_Uninit(Group(range(0, o, 1), fun k -> &t[MINDEX3(m, n, o, i, j, k)] ~> Cell))");
      __produces("Group(range(0, o, 1), fun k -> &t[MINDEX3(m, n, o, i, j, k)] ~> Cell)");
      for ( int k = 0; k < o; k++ ) {
        __sequentially_reads("&x ~> Cell");
        __consumes("_Uninit(&t[MINDEX3(m, n, o, i, j, k)] ~> Cell)");
        __produces("&t[MINDEX3(m, n, o, i, j, k)] ~> Cell");
        t[MINDEX3(m, n, o, i, j, k)] = x;
      }
    }
  }
  for ( int i = 0; i < m; i++ ) {
    __reads("Group(range(0, n, 1), fun j -> Group(range(0, o, 1), fun k -> &t[MINDEX3(m, n, o, i, j, k)] ~> Cell))");
    for ( int j = 0; j < n; j++ ) {
      __reads("Group(range(0, o, 1), fun k -> &t[MINDEX3(m, n, o, i, j, k)] ~> Cell)");
      for ( int k = 0; k < o; k++ ) {
        __reads("&t[MINDEX3(m, n, o, i, j, k)] ~> Cell");
        int y = t[MINDEX3(m, n, o, i, j, k)];
      }
    }
  }
}

  void ghost_scope ( int m, int n )  {
  __pure();
  int x = 0;
  for ( int i = 0; i < m; i++ ) {
    __sequentially_reads("&x ~> Cell");
      const  __ghost_fn xfg = __ghost_begin(ro_fork_group, "H := &x ~> Cell, r := range(0, n, 1)");
    for ( int j = 0; j < n; j++ ) {
      __reads("&x ~> Cell");
      int y = x;
    }
    __ghost_end(xfg);
  }
  for ( int i = 0; i < m; i++ ) {
    __sequentially_reads("&x ~> Cell");
      const  __ghost_fn __ghost_pair_1 = __ghost_begin(ro_fork_group, "H := &x ~> Cell, r := range(0, n, 1)");
    for ( int j = 0; j < n; j++ ) {
      __reads("&x ~> Cell");
      int z = x;
    }
    __ghost_end(__ghost_pair_1);
  }
}
