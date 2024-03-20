#include <optitrust.h>

void demo_both_par(int* t, int n, int m) {
  __modifies("for i in 0..n -> for j in 0..m ->"
             " &t[(i * m + j)] ~> Cell");
  for (int i = 0; i < n; i++) {
    __strict();
    __modifies("for j in 0..m -> &t[(i * m + j)] ~> Cell");
    for (int j = 0; j < m; j++) {
      __strict();
      __modifies("&t[(i * m + j)] ~> Cell");
      t[i * m + j] = j;
    }
  }
}

void demo_outer_par(int* t, int n) {
  __modifies("for i in 0..n -> &t[i] ~> Cell");

  for (int i = 0; i < n; i++) {
    __strict();
    __modifies("&t[i] ~> Cell");

    for (int j = 0; j < 4; j++) {
      __strict();
      __sequentially_modifies("&t[i] ~> Cell");
      t[i] = j;
    }
  }
}

void g(int* t) {
  __modifies("t ~> Matrix3(7, 10, 20)");

  for (int a = 0 ; a < 7; a++) {
    __modifies("for b in 0..10 ->"
               "for c in 0..20 -> &t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell");
    for(int b = 0; b < 10; b++) {
      __modifies("for c in 0..20 -> &t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell");
      for(int c = 0; c < 20; c++){
        __modifies("&t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell");
        t[MINDEX3(7, 10, 20, a, b, c)] = 0;
      }
    }
  }

  for (int i = 0; i < 10; i++) {
    __strict();
    for (int j = i; j < i + 1; j++) {
      __strict();

    }
  }
}

void f(int* t, int* u, int* v, int n, int m) {
  __modifies("t ~> Matrix1(n)");
  __modifies("v ~> Matrix2(n, m)");
  __reads("u ~> Matrix1(n)");

  for (int x = 0 ; x < n; x++) {
    __strict();
    __modifies("&t[MINDEX1(n,x)] ~> Cell");
    __modifies("for y in 0..m -> &v[MINDEX2(n,m,x,y)] ~> Cell");
    __reads("&u[MINDEX1(n,x)] ~> Cell");

    for (int y = 0; y < m; y++) {
      __strict();
      __modifies("&v[MINDEX2(n,m,x,y)] ~> Cell");
      __sequentially_modifies("&t[MINDEX1(n,x)] ~> Cell");
      __parallel_reads("&u[MINDEX1(n,x)] ~> Cell");

      t[MINDEX1(n,x)] = y * u[MINDEX1(n,x)];
      v[MINDEX2(n,m,x,y)] = t[MINDEX1(n,x)];
    }
  }
}

void par_reads() {
  __pure();

  int x = 0;
  for (int i = 0; i < 5; i++) {
    __strict();
    __parallel_reads("&x ~> Cell");
    for (int j = 0; j < 5; j++) {
      __strict();
      __parallel_reads("&x ~> Cell");
      x + 1;
    }
  }
}

void indep_reads(int* M) {
  __reads("M ~> Matrix2(5, 5)");

  for (int i = 0; i < 5; i++) {
    __strict();
    __reads("for j in 0..5 -> &M[MINDEX2(5,5,i,j)] ~> Cell");
    for (int j = 0; j < 5; j++) {
      __strict();
      __reads("&M[MINDEX2(5,5,i,j)] ~> Cell");
      M[MINDEX2(5,5,i,j)];
    }
  }
}

void ghost_pairs(int* x) {
  __reads("x ~> Matrix1(1)");

  for (int i = 0; i < 5; i++) {
    __strict();
    __parallel_reads("x ~> Matrix1(1)");
    __GHOST_BEGIN(focus_x, matrix1_ro_focus, "i := 0");
    for (int j = 0; j < 5; j++) {
      __strict();
      __parallel_reads("&x[MINDEX1(1,0)] ~> Cell");
      x[MINDEX1(1,0)] + 1;
    }
    __GHOST_END(focus_x);
  }
}
