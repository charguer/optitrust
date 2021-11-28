#include <stdio.h>

int p = 5;

typedef struct {
  int x;
  int y;
} vect;

int test() { return 0; }

typedef vect myvect;

int main() {
  int a = 5;
  float const b = 5.;
  int x = 3;
  int y = 1;
  printf("%d", y);
  p++;
  y++;
  for (int i = 0; (i < 5); i++) {
    x++;
  }
  int z = 5;
  return 0;
}
