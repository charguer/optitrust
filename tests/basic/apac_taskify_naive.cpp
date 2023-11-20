#include <stdlib.h>

int f(int a, int b) {
  return a + b;
}

int g(int * a) {
  *a = 2;
  return 0;
}

int h() {
  int a;
  int b = 0;
  int * c = (int *) malloc(sizeof(int));
  a = 1 + b++;
  b++;
  g(c);
  a = 2;
  f(a, b);
  a = 3;
}
