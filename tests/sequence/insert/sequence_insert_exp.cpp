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
  p++;
  const float b = 5.;
  p++;
  int x = 3;
  p++;
  int y = 1;
  p++;
  printf("%d", y);
  p++;
  y++;
  for (int i = 0; i < 5; i++) {
    x++;
  }
  int z = 5;
  p++;
  return 0;
}
