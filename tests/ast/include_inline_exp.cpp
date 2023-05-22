#include <stdio.h>

typedef struct {
  int x;
  int y;
} particle;

typedef struct {
  particle items[2];
} bag;

int f(int x) { return x; }

bag b;

int main() {
  int x = f(5);
  printf("%d\n", x);
  return 0;
}
