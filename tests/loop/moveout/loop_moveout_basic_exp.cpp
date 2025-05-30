#include <optitrust.h>

void var(int* t) {
  __modifies("t ~> Matrix1(3)");
  int x = 0;
  x = 3;
  for (int i = 0; i < 3; i++) {
    __strict();
    __smodifies("&x ~> Cell");
    __xmodifies("&t[MINDEX1(3, i)] ~> Cell");
    t[MINDEX1(3, i)] = x;
  }
}

void var_wrong1(int* t) {
  __modifies("t ~> Matrix1(3)");
  int x = 0;
  for (int i = 0; i < 3; i++) {
    __strict();
    __smodifies("&x ~> UninitCell");
    __xmodifies("&t[MINDEX1(3, i)] ~> Cell");
    x = i;
    t[MINDEX1(3, i)] = x;
  }
}

void var_wrong2(int* t) {
  __modifies("t ~> Matrix1(3)");
  int x = 0;
  for (int i = 0; i < 3; i++) {
    __strict();
    __smodifies("&x ~> UninitCell");
    __xmodifies("&t[MINDEX1(3, i)] ~> Cell");
    t[MINDEX1(3, i)] = 0;
    x = 3;
    t[MINDEX1(3, i)] += x;
  }
}

void var_wrong3(int* t) {
  __modifies("t ~> Matrix1(3)");
  int x = 0;
  for (int i = 0; i < 3; i++) {
    __strict();
    __smodifies("&x ~> Cell");
    __xmodifies("&t[MINDEX1(3, i)] ~> Cell");
    x += 3;
    t[MINDEX1(3, i)] = x;
  }
}

void var_wrong4(int* t) {
  __modifies("t ~> Matrix1(3)");
  int x = 0;
  for (int i = 0; i < 3; i++) {
    __strict();
    __smodifies("&x ~> UninitCell");
    __xmodifies("&t[MINDEX1(3, i)] ~> Cell");
    x = 3;
    x++;
    t[MINDEX1(3, i)] = x;
  }
}

void var_needs_if(int* t, int n) {
  __modifies("t ~> Matrix1(n)");
  int x = 0;
  if (0 < n) x = 3;
  for (int i = 0; i < n; i++) {
    __strict();
    __smodifies("&x ~> Cell");
    __xmodifies("&t[MINDEX1(n, i)] ~> Cell");
    t[MINDEX1(n, i)] = x;
  }
  int y = x;
}

void arr(int* t, int* x) {
  __modifies("t ~> Matrix2(3, 5)");
  __modifies("x ~> UninitMatrix1(5)");
  for (int j = 0; j < 5; j++) {
    __strict();
    __xwrites("&x[MINDEX1(5, j)] ~> Cell");
    x[MINDEX1(5, j)] = 3;
  }
  for (int i = 0; i < 3; i++) {
    __strict();
    __smodifies("x ~> Matrix1(5)");
    __xmodifies("for j in 0..5 -> &t[MINDEX2(3, 5, i, j)] ~> Cell");
    for (int j = 0; j < 5; j++) {
      __strict();
      __xwrites("&t[MINDEX2(3, 5, i, j)] ~> Cell");
      __xreads("&x[MINDEX1(5, j)] ~> Cell");
      t[MINDEX2(3, 5, i, j)] = x[MINDEX1(5, j)];
    }
  }
}

void arr_wrong1(int* t, int* x) {
  __modifies("t ~> Matrix2(3, 5)");
  __modifies("x ~> UninitMatrix1(5)");
  int v = 3;
  for (int i = 0; i < 3; i++) {
    __strict();
    __smodifies("x ~> UninitMatrix1(5)");
    __smodifies("&v ~> Cell");
    __xmodifies("for j in 0..5 -> &t[MINDEX2(3, 5, i, j)] ~> Cell");
    for (int j = 0; j < 5; j++) {
      __strict();
      __sreads("&v ~> Cell");
      __xwrites("&x[MINDEX1(5, j)] ~> Cell");
      x[MINDEX1(5, j)] = v;
    }
    for (int j = 0; j < 5; j++) {
      __strict();
      __xwrites("&t[MINDEX2(3, 5, i, j)] ~> Cell");
      __xreads("&x[MINDEX1(5, j)] ~> Cell");
      t[MINDEX2(3, 5, i, j)] = x[MINDEX1(5, j)];
    }
    v++;
  }
}

void test(int* t) {
  __modifies("t ~> UninitMatrix1(10)");
  int a = 5;
  int b = 6;
  for (int i = 0; i < 10; i++) {
    __strict();
    __sreads("&a ~> Cell");
    __sreads("&b ~> Cell");
    __xmodifies("&t[MINDEX1(10, i)] ~> UninitCell");
    int r = i;
    int s = i;
    int x = a + b;
    for (int j = 0; j < 10; j++) {
      __strict();
      __smodifies("&x ~> Cell");
      __smodifies("&s ~> Cell");
      __smodifies("&t[MINDEX1(10, i)] ~> UninitCell");
      __sreads("&a ~> Cell");
      __sreads("&b ~> Cell");
      t[MINDEX1(10, i)] = i;
    }
  }
}
