
typedef struct { int x; int y; } vect;

typedef struct vect2 { int x; int y; } vect2;

typedef struct items { int head; struct items* tail; } items;

int main() {
  return 0;
}

/* typedef struct { int x; int y; } vect;

int main() {
  vect v = { 2, 4 };
  v.x;
}

const int x, y = 4;
const int z = x * y;
const double t = 2.0 * x;

#include "stdbool.h"
#include "stdio.h"



int main() {
  label:;
  bool b = true;
  printf("5");
}

*/

/*
#include "stdlib.h"

int main() {
  int nb = 5;
  int* t = (int*) malloc(nb*sizeof(int));
}
*/

// gcc -std=c99 c_small_test.cpp
// gcc -std=c++11 c_small_test.cpp