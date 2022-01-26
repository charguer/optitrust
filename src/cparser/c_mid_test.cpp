const int CHUNKSIZE = 128;
int t[CHUNKSIZE];
int u[2*CHUNKSIZE+1];

struct list {
  int head;
  struct list* tail;
};

/*
typedef struct tlist {
  int head;
  tlist* tail;
} tlist;
  gcc -std=c99 c_small_test.cpp

*/

typedef struct vect {
  double x;
  double y;
  double z;
} vect;

vect vect_mul(double d, vect v) {
  return {(d * v.x), (d * v.y), (d * v.z)};
}

int testref() {
  int a = 0;
  int& b = a;
  int* p = &a;
  int* & q = p;
  int* const & r = q;
  *p += *r;
  const int c = 0;
  const int & s = c;
  const int & t = a;
  return b + 1;
}

// #include "stdlib.h"

void* malloc(int);

int f(int x) {
  int a = x + x;
  return a + a;
}

int g(int x) {
  if (x > 0)
    return 1;
  else
    return 2;
}

int h(int x) {
  if (x > 0)
    return 1;
  return 2;
}

void m(int* p) {
  (*p)++;
}

int k(int a, int b) {
  return a + b;
}

int main(){
  int x = 3;
  int y = f(x);
  int z = g(x);
  int u = h(x);
  // int *q = new int(3);
  int *q = (int*) malloc(sizeof(int));
  *q = 3;
  m(q);
  int result = k(result, 4);
  return 0;
}

int testfun(int f(int a), int (*g)(int)) {
  int x = 1;
  return f(x) + g(x);
}
