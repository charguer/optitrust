#include <stdio.h>
int min(int x, int y) {
  return (x < y) ? x : y;
}

int main() {
  int total1 = 0, total2 = 0, total3 = 0;
  for (int x = 0; x < 10; x++) {
    total1 += x;
  }
  printf ("%d\n", total1);
  // Does it make sense to tile a loop which does not iterate continuously?
  for (int y = 0; y < 9; y += 3) {
    total2 += y;
  }
  printf ("%d\n", total2);

  for (int z = 0; z < 9; z++) {
    total3 += z;
  }
  printf ("%d\n", total3);

  return 0;
}