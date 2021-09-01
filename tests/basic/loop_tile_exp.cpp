#include <stdio.h>

int min(int x, int y) { return ((x < y) ? x : y); }

int main() {
  int total1 = 0, total2 = 0, total3 = 0;
  for (int bx = 0; (bx < 10); bx += 2) {
    for (int x = bx; (x < (bx + 2)); x++) {
      total1 += x;
    }
  }
  printf("%d\n", total1);
  for (int by = 0; (by < 9); by += 2) {
    for (int y = by; (y < min(9, (by + 2))); y += 3) {
      total2 += y;
    }
  }
  printf("%d\n", total2);
  for (int bz = 0; (bz < 9); bz += 2) {
    for (int z = bz; ((z < (bz + 2)) && (z < 9)); z++) {
      total3 += z;
    }
  }
  printf("%d\n", total3);
  return 0;
}