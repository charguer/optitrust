

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

void iter_nat_for(int n, void body(int)) {
  for (int i = 0; i < n; i++) {
    body(i);
  }
}

void test_ho() {
  int s = 0;
  int m = 3;
  hobody: {
    for (int j = 0; j < m; j++) {
      {
        s += 2*j;
        s -= j;
      }
    }
  }
}

#include <stdio.h>

typedef struct { } particle;
typedef struct { } bag;
typedef struct { } bag_iter;
bag_iter* bag_iter_begin(bag* b);
particle* bag_iter_get(bag_iter* it);
particle* bag_iter_next(bag_iter* it, bool destructive);

void iter_bag(bag* b, void body(particle*)) {
  bag_iter* it = bag_iter_begin(b);
  for (particle* p = bag_iter_get(it); p != NULL; p = bag_iter_next(it, true)) {
    body(p);
  }
}

void test_bag() {
  bag* mybag;
  bagbody: {
    // This is the code pattern to use in pic_demo
    bag_iter* myit = bag_iter_begin(mybag);
    for (particle* p = bag_iter_get(myit); p != NULL; p = bag_iter_next(myit, true)) {
      {
         if (p = p) { return; }
      }
    }
  }
}


/* LATER: due to the encodings, there is some mismatch in this code

typedef struct { } particle;
typedef struct { } bag;
typedef struct { } bag_iter;
bag_iter bag_iter_begin(bag* b);
particle* bag_iter_get(bag_iter* it);
particle* bag_iter_next(bag_iter* it, bool destructive);

void iter_bag(bag* b, void body(particle*)) {
  bag_iter it = bag_iter_begin(b);
  for (particle* p = bag_iter_get(&it); p != NULL; p = bag_iter_next(&it, true)) {
    body(p);
  }
}

void test_bag() {
  bag* mybag;
  bagbody: {
    bag_iter myit = bag_iter_begin(mybag);
    for (particle* p = bag_iter_get(&myit); p != NULL; p = bag_iter_next(&myit, true)) {
      {
         if (p = p) { return; }
      }
    }
  }
}

*/