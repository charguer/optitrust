#include <stdio.h>

int min(int x, int y) {
  return (x < y) ? x : y;
}

int main() {
  int s1 = 0;
  int s2 = 0;
  int s3 = 0;
  for (int x = 0; x < 10; x++) {
    s1 += x;
  }
  for (int y = 0; y < 9; y++) {
    s2 += y;
  }
  for (int z = 0; z < 9; z++) {
    s3 += z;
  }
  printf ("%d %d %d\n", s1, s2, s3);

  int t1 = 0;
  int t2 = 0;
  int t3 = 0;
  for (int i = 0; i < 12; i+=3) {
    t1 += i;
  }
  for (int j = 0; j < 13; j+=3) {
    t2 += j;
  }
  for (int k = 0; k < 13; k+=3) {
    t3 += k;
  }
  printf ("%d %d %d\n", t1, t2, t3);
  return 0;
}
