#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

void g(int x, int y) {}

void gtwice(int x) { g(x, x); }

void test_trivial() {
gtwice_body:
  gtwice(3);
}

void f(int x) {
  int a = x + 1;
  g(a, x);
}

void test_basic() {
  int r = 5;
fbody:
  f(r + 2);
}

void test_basic2() {
  const int r = 5;
fbody:
  f(r);
}

void test_basic3() {
  int r = 5;
fbody:
  f(r);
}

void iter_nat_for(int n, void body(int)) {
  for (int i = 0; i < n; i++) {
    body(i);
  }
}

void test_ho() {
  int s = 0;
  int m = 3;
hobody:
  iter_nat_for(
      m, void body(int i) {
        s += 2 * j;
        s -= j;
      });
}

typedef struct {
  int pos;
} particle;

typedef struct {
} bag;

typedef struct {
} bag_iter;

bag_iter* bag_iter_begin(bag* b);

particle* bag_iter_get(bag_iter* it);

particle* bag_iter_next(bag_iter* it, bool destructive);

void iter_bag(bag* b, void body(particle*)) {
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
  bag_iter* const iter = bag_iter_begin(mybag);
  for (particle* p = bag_iter_get(iter); p != NULL;
       p = bag_iter_next(iter, true)) {
    p->pos = p->pos + 1;
  }
  free(iter);
}

particle* bag2_iter_begin(bag_iter* it, bag* b);

particle* bag2_iter_next(bag_iter* it, bool destructive);

void iter_bag2(bag* b, void body(particle*)) {
  bag_iter iter;
  for (particle* p = bag2_iter_begin(&iter, b); p != NULL;
       p = bag2_iter_next(&iter, true)) {
    body(p);
  }
}

void test_bag2() {
  int x = 0;
  bag* mybag;
  bag_iter iter;
  for (particle* p = bag2_iter_begin(&iter, mybag); p != NULL;
       p = bag2_iter_next(&iter, true)) {
    p->pos = p->pos + 1;
  }
}
