#include <optitrust.h>

void g(int* t) {
  __modifies("t ~> Matrix3(7, 10, 20)");

  for (int a = 0 ; a < 7; a++) {
    __modifies("Group(range(0, 10, 1), fun b ->"
               "Group(range(0, 20, 1), fun c -> &t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell))");
    for(int b = 0; b < 10; b++) {
      __modifies("Group(range(0, 20, 1), fun c -> &t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell)");
      for(int c = 0; c < 20; c++){
        __modifies("&t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell");
        t[MINDEX3(7, 10, 20, a, b, c)] = 0;
      }
    }
  }

  for (int i = 0; i < 10; i++) {
    __pure();
    for (int j = i; j < i + 1; j++) {
      __pure();

    }
  }
}

void f(int* t, int* u, int* v, int n, int m) {
  __modifies("t ~> Matrix1(n)");
  __modifies("v ~> Matrix2(n, m)");
  __reads("u ~> Matrix1(n)");

  for (int x = 0 ; x < n; x++) {
    __modifies("&t[MINDEX1(n,x)] ~> Cell");
    __modifies("Group(range(0,m,1), fun y -> &v[MINDEX2(n,m,x,y)] ~> Cell)");
    __reads("&u[MINDEX1(n,x)] ~> Cell");

    for (int y = 0; y < m; y++) {
      __modifies("&v[MINDEX2(n,m,x,y)] ~> Cell");
      __sequentially_modifies("&t[MINDEX1(n,x)] ~> Cell");
      __sequentially_reads("&u[MINDEX1(n,x)] ~> Cell");

      t[MINDEX1(n,x)] = y * u[MINDEX1(n,x)];
      v[MINDEX2(n,m,x,y)] = t[MINDEX1(n,x)];
    }
  }
}

void seq_reads() {
  __pure();

  int x = 0;
  for (int i = 0; i < 5; i++) {
    __sequentially_reads("&x ~> Cell");
    for (int j = 0; j < 5; j++) {
      __sequentially_reads("&x ~> Cell");
      x + 1;
    }
  }
}

void ghost_pairs() {
  __pure();

  int x = 0;
  for (int i = 0; i < 5; i++) {
    __sequentially_reads("&x ~> Cell");
    for (int j = 0; j < 5; j++) {
      __sequentially_reads("&x ~> Cell");
      for (int k = 0; j < 5; k++) {
        __sequentially_reads("&x ~> Cell");
        x + 1;
      }
    }
  }
}
