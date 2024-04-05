#include <optitrust.h>
#include <stdio.h>

int min(int x, int y) { return (x < y ? x : y); }

void f() {
  int s1 = 0;
  int s2 = 0;
  int s3 = 0;
  for (int bx = 0; bx < 5; bx++) {
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
}

void matrix_copy(int* D, int* S) {
  __modifies("D ~> Matrix1(1024)");
  __reads("S ~> Matrix1(1024)");
  __ghost(tile_divides,
          "tile_count := 256, tile_size := 4, size := 1024, items := fun i -> "
          "&D[MINDEX1(1024, i)] ~> Cell");
  for (int bi = 0; bi < 256; bi++) {
    __strict();
    __sreads("S ~> Matrix1(1024)");
    __xmodifies("for i in 0..4 -> &D[MINDEX1(1024, bi * 4 + i)] ~> Cell");
    for (int i = 0; i < 4; i++) {
      __strict();
      __sreads("S ~> Matrix1(1024)");
      __xmodifies("&D[MINDEX1(1024, bi * 4 + i)] ~> Cell");
      __ghost(tiled_index_in_range,
              "tile_index := bi, index := i, tile_count := 256, tile_size := "
              "4, size := 1024");
      const __ghost_fn focus =
          __ghost_begin(matrix1_ro_focus, "M := S, i := bi * 4 + i");
      D[MINDEX1(1024, bi * 4 + i)] = S[MINDEX1(1024, bi * 4 + i)];
      __ghost_end(focus);
    }
  }
  __ghost(untile_divides,
          "tile_count := 256, tile_size := 4, size := 1024, items := fun i -> "
          "&D[MINDEX1(1024, i)] ~> Cell");
}
