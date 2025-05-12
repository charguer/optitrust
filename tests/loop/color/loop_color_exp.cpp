#include <optitrust.h>
#include <stdio.h>

int main() {
  int N = 10;
  int C = 2;
  int D = 2;
  int s = 0;
  for (int ci = 0; ci < C; ci++) {
    for (int i = ci; i < N; i += C) {
      s += i;
    }
  }
  for (int cj = 0; cj < C; cj++) {
    for (int j = cj * 2; j < N; j += C * 2) {
      s += j;
    }
  }
  printf("%d\n", s);
  return 0;
}

void matrix_copy(int* D, int* S) {
  __modifies("D ~> Matrix1(1024)");
  __reads("S ~> Matrix1(1024)");
  __ghost(color,
          "nb_colors := 2, size := 1024, items := fun (k: int) -> "
          "&D[MINDEX1(1024, k)] ~> Cell");
  for (int ck = 0; ck < 2; ck++) {
    __strict();
    __sreads("S ~> Matrix1(1024)");
    __xmodifies("for k in range(ck, 1024, 2) -> &D[MINDEX1(1024, k)] ~> Cell");
    for (int k = ck; k < 1024; k += 2) {
      __strict();
      __sreads("S ~> Matrix1(1024)");
      __xmodifies("&D[MINDEX1(1024, k)] ~> Cell");
      __ghost(colored_index_in_range_step1,
              "color_index := ck, index := k, nb_colors := 2, size := 1024");
      const __ghost_fn focus =
          __ghost_begin(ro_matrix1_focus, "matrix := S, i := k");
      D[MINDEX1(1024, k)] = S[MINDEX1(1024, k)];
      __ghost_end(focus);
    }
  }
  __ghost(uncolor,
          "nb_colors := 2, size := 1024, items := fun (k: int) -> "
          "&D[MINDEX1(1024, k)] ~> Cell");
}
