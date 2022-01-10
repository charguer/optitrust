#include <stdio.h>

void g(int x, int y) {}

void gtwice(int x) { g(x, x); }

void test_trivial() {
gtwice_body:
  gtwice(3);
}

void f(int x) {
  int a = (x + 1);
  g(a, x);
}

void test_basic() {
  int r = 5;
fbody:
  f((r + 2));
}

void iter_nat_for(int n, @body) {
  for (int i = 0; (i < n); i++) {
    body(i);
  }
}

void test_ho() {
  int s = 0;
  int m = 3;
hobody:
  iter_nat_for(
      m, void body(int j) {
        s += (2 * j);
        s -= j;
      });
}

typedef struct {
} particle;

typedef struct {
} bag;

typedef struct {
} bag_iter;

bag_iter *bag_iter_begin(bag *b);

particle *bag_iter_get(bag_iter *it);

particle *bag_iter_next(bag_iter *it, bool destructive);

void iter_bag(bag *b, @body) {
  bag_iter *it = bag_iter_begin(b);
  for (particle *p = bag_iter_get(it); (p != NULL);
       p = bag_iter_next(it, true)) {
    body(p);
  }
}

void test_bag() {
  bag *mybag;
bagbody:
  iter_bag(
      mybag, void body(particle * *p) {
        if (p = p) {
          return;
        }
      });
}
