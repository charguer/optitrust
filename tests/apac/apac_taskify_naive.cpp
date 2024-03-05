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
  int b;
  b = 0;
  b = b + 1;
  b--;
  int ** c;
  c = (int **) malloc(sizeof(int));
  a = 1 + b++;
  b++;
  g(*c);
  a = 2;
  f(a, b);
  a = 3;
  **c = a;
}
