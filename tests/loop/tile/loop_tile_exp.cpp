#include <stdio.h>

int min(int x, int y) { return (x < y ? x : y); }

int main() {
  int s1 = 0;
  int s2 = 0;
  int s3 = 0;
  for (int bx = 0; bx < exact_div(10, 2); bx++) {
    for (int x = 0; x < 2; x++) {
      s1 += bx * 2 + x;
    }
  }
  for (int by = 0; by < 9; by += 2) {
    for (int y = by; y < min(9, by + 2); y++) {
      s2 += y;
    }
  }
  for (int bz = 0; bz < 9; bz += 2) {
    for (int z = bz; z < bz + 2 && z < 9; z++) {
      s3 += z;
    }
  }
  printf("%d %d %d\n", s1, s2, s3);
  int t1 = 0;
  int t2 = 0;
  int t3 = 0;
  for (int bi = 0; bi < exact_div((12 / 3), 2); bi++) {
    for (int i = 0; i < 2; i++) {
      t1 += (bi * 2 + i) * 3;
    }
  }
  for (int bj = 0; bj < 13; bj += 2 * 3) {
    for (int j = bj; j < min(13, bj + 2 * 3); j += 3) {
      t2 += j;
    }
  }
  for (int bk = 0; bk < 13; bk += 2 * 3) {
    for (int k = bk; k < bk + 2 * 3 && k < 13; k += 3) {
      t3 += k;
    }
  }
  printf("%d %d %d\n", t1, t2, t3);
  return 0;
}
