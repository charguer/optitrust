#include <stdio.h>
int min(int x, int y) {
  return (x < y) ? x : y;
}

int main() {
  int s1 = 0, s2 = 0, s3 = 0;
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

  int t1 = 0, t2 = 0, t3 = 0;
  for (int a = 0; a < 12; a+=3) {
    t1 += a;
  }
  for (int b = 0; b < 13; b+=3) {
    t2 += b;
  }
  for (int c = 0; c < 13; c+=3) {
    t3 += c;
  }
  printf ("%d %d %d\n", t1, t2, t3);
  return 0;
}