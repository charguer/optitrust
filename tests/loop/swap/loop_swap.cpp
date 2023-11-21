#include <optitrust.h>

int* t;

int main() {
  for (int a = 0 ; a < 7; a++) {
    for(int b = 0; b < 10; b++) {
      for(int c = 0; c < 20; c++){
        t[a] = b;
      }
    }
  }

  for (int i = 0; i < 10; i++) {
    for (int j = i; j < i + 1; j++) {

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
