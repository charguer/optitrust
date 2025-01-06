#include <optitrust.h>
#include <stdio.h>
#include <stdlib.h>

void g(int x, int y) { __pure(); }

void gtwice(int x) {
  __pure();
  g(x, x);
}

void test_trivial() {
  __pure();
gtwice_start:;
  gtwice(3);
gtwice_end:;
}

void f(int x) {
  __pure();
  int a = x + 1;
  g(a, x);
}

void test_basic() {
  __pure();
  int r = 5;
f_start:;
  f(r + 2);
f_end:;
}

void test_basic2() {
  __pure();
  const int r = 5;
f_start:;
  f(r);
f_end:;
}

void test_basic3() {
  __pure();
  int r = 5;
f_start:;
  f(r);
f_end:;
}

int loop_with_ret(int n, int v) {
  __pure();
  int s = 0;
  for (int i = 0; i < n; i++) {
    __strict();
    __smodifies("&s ~> Cell");
    s += v;
  }
  __admitted();
  return s;
}

void call_loop_with_ret() {
  __pure();
  int ret;
loop_with_ret_start:;
  ret = loop_with_ret(7, 7 + 4);
loop_with_ret_end:;
}

void iter_nat_for(int n, void (*body)(int)) {
  for (int i = 0; i < n; i++) {
    body(i);
  }
}

void test_ho() {
  int s = 0;
  int m = 3;
hobody : {
  for (int j = 0; j < m; j++) {
    {
      s += 2 * j;
      s -= j;
    }
  }
}
}

typedef struct {
  int pos;
} particle;

typedef struct {
} bag;

typedef struct {
} bag_iter;

bag_iter* bag_iter_begin(bag*);

particle* bag_iter_get(bag_iter*);

particle* bag_iter_next(bag_iter*, bool);

void iter_bag(bag* b, void (*body)(particle*)) {
  bag_iter* const iter = bag_iter_begin(b);
  for (particle* p = bag_iter_get(iter); p != NULL;
       p = bag_iter_next(iter, true)) {
    body(p);
  }
  free(iter);
}

void test_bag() {
  int x = 0;
  bag* mybag;
bagbody : {
  bag_iter* const myit = bag_iter_begin(mybag);
  for (particle* p = bag_iter_get(myit); p != NULL;
       p = bag_iter_next(myit, true)) {
    { p->pos = p->pos + 1; }
  }
  free(myit);
}
}

particle* bag2_iter_begin(bag_iter*, bag*);

particle* bag2_iter_next(bag_iter*, bool);

void iter_bag2(bag* b, void (*body)(particle*)) {
  bag_iter iter;
  for (particle* p = bag2_iter_begin(&iter, b); p != NULL;
       p = bag2_iter_next(&iter, true)) {
    body(p);
  }
}

void test_bag2() {
  int x = 0;
  bag* mybag;
bagbody2 : {
  bag_iter myit;
  for (particle* p = bag2_iter_begin(&myit, mybag); p != NULL;
       p = bag2_iter_next(&myit, true)) {
    { p->pos = p->pos + 1; }
  }
}
}
