#include <stdio.h>
#include <optitrust.h>

int main(){
  int N = 10;
  int C = 2;
  int D = 2;
  int s = 0;
  for (int i = 0; i < N; i++) {
    s += i;
  }

  for (int j = 0; j < N; j += 2) {
    s += j;
  }
  printf("%d\n", s);
    /**/
  return 0;
}
/*
*/


void matrix_copy(int* D, int* S) {
  __modifies("D ~> Matrix1(1024)");
  __reads("S ~> Matrix1(1024)");

  for (int k = 0; k < 1024; ++k) {
    __strict();
    __xmodifies("&D[MINDEX1(1024, k)] ~> Cell");
    __sreads("S ~> Matrix1(1024)");
    __GHOST_BEGIN(focus, ro_matrix1_focus, "S, k");
    D[MINDEX1(1024, k)] = S[MINDEX1(1024, k)];
    __GHOST_END(focus);
  }
}
