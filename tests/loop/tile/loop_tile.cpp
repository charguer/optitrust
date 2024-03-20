#include <stdio.h>
#include <optitrust.h>

int min(int x, int y) {
  return (x < y) ? x : y;
}

void f() {
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
}

void matrix_copy(int* D, int* S) {
  __modifies("D ~> Matrix1(1024)");
  __reads("S ~> Matrix1(1024)");

  for (int i = 0; i < 1024; ++i) {
    __strict();
    __modifies("&D[MINDEX1(1024, i)] ~> Cell");
    __parallel_reads("S ~> Matrix1(1024)");
    __GHOST_BEGIN(focus, matrix1_ro_focus, "S, i");
    D[MINDEX1(1024, i)] = S[MINDEX1(1024, i)];
    __GHOST_END(focus);
  }
}
