#include <stdbool.h>

void g(int x, int y) {}

void gtwice(int x) {
  g(x, x);
}

void test_trivial() {
  gtwice_body: { g(3, 3); }
}

void f(int x) {
  int a = x+1;
  g(a, x);
}

void test_basic() {
  int r = 5;
  fbody:{
    int b = (r+2)+1;
    g(b, r+2);
  }
}

void test_basic2() {
  const int r = 5;
  fbody:{
    int b = r+1;
    g(b, r);
  }
}


void test_basic3() {
  int r = 5;
  fbody:{
    int b = r+1;
    g(b, r);
  }
}

void iter_nat_for(int n, void body(int)) {
  for (int i = 0; i < n; i++) {
    body(i);
  }
}

#include <stdio.h>
#include <stdlib.h>

typedef struct { int pos; } particle;
typedef struct { } bag;
typedef struct { } bag_iter;
bag_iter* bag_iter_begin(bag* b);
particle* bag_iter_get(bag_iter* it);
particle* bag_iter_next(bag_iter* it, bool destructive);

void iter_bag(bag* b, void body(particle*)) {
  bag_iter* const iter = bag_iter_begin(b);
  for (particle* p = bag_iter_get(iter); p != NULL; p = bag_iter_next(iter, true)) {
    body(p);
  }
  free(iter);
}

void test_bag() {
  int x = 0;
  bag* mybag;
  bag_iter* const myit = bag_iter_begin(mybag);
  particle* p = bag_iter_get(myit);
  
  p->pos = p->pos + 1;
  free(myit);
}
