#include <stdio.h>

int p = 5;


int f();

int main() {
  int a = 5;
  const float b = 5.;
  int x = 3;
  int y = 1;
  printf("%d", y);
  p++;
  y++;
  for (int i = 0; i < 5; i++) {
    x++;
  }
  int z = 5;
  return 0;
}
