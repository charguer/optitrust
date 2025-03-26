// File obtained by taking the matmul_check_after.cpp
// with execution on the last line of script before Cleanup.std
// with flag:  Flags.pretty_matrix_notation := true

#include <optitrust.h>

void mm1024(float* C, float* A, float* B) {
  __modifies("C ~> Matrix2(1024, 1024)");
  __reads("A ~> Matrix2(1024, 1024)");
  __reads("B ~> Matrix2(1024, 1024)");
  __ghost(tile_divides,
          "tile_count := 32, tile_size := 32, size := 1024, items := fun i -> "
          "for j in 0..1024 -> &C[MINDEX2(1024, 1024, i, j)] ~> Cell");
  float* const pB = MALLOC4(float, 32, 256, 4, 32);
  for (int bj = 0; bj < 32; bj++) {
    __strict();
    __sreads("B ~> Matrix2(1024, 1024)");
    __xwrites(
        "for _v9 in 0..256 -> for _v10 in 0..4 -> for _v11 in 0..32 -> "
        "&pB[MINDEX4(32, 256, 4, 32, bj, _v9, _v10, _v11)] ~> Cell");
    for (int bk = 0; bk < 256; bk++) {
      __strict();
      __sreads("B ~> Matrix2(1024, 1024)");
      __xwrites(
          "for _v7 in 0..4 -> for _v8 in 0..32 -> &pB[MINDEX4(32, 256, 4, 32, "
          "bj, bk, _v7, _v8)] ~> Cell");
      for (int k = 0; k < 4; k++) {
        __strict();
        __sreads("B ~> Matrix2(1024, 1024)");
        __xwrites(
            "for _v6 in 0..32 -> &pB[MINDEX4(32, 256, 4, 32, bj, bk, k, _v6)] "
            "~> Cell");
        for (int j = 0; j < 32; j++) {
          __strict();
          __sreads("B ~> Matrix2(1024, 1024)");
          __xwrites("&pB[MINDEX4(32, 256, 4, 32, bj, bk, k, j)] ~> Cell");
          __ghost(tiled_index_in_range,
                  "tile_index := bj, index := j, tile_count := 32, tile_size "
                  ":= 32, size := 1024");
          __ghost(tiled_index_in_range,
                  "tile_index := bk, index := k, tile_count := 256, tile_size "
                  ":= 4, size := 1024");
          const __ghost_fn __ghost_pair_3 = __ghost_begin(
              ro_matrix2_focus, "M := B, i := bk * 4 + k, j := bj * 32 + j");
          pB[MINDEX4(32, 256, 4, 32, bj, bk, k, j)] =
              B[MINDEX2(1024, 1024, bk * 4 + k, bj * 32 + j)];
          __ghost_end(__ghost_pair_3);
        }
      }
    }
  }
  for (int bi = 0; bi < 32; bi++) {
    __strict();
    __sreads("pB ~> Matrix4(32, 256, 4, 32)");
    __sreads("A ~> Matrix2(1024, 1024)");
    __xmodifies(
        "for i in 0..32 -> for j in 0..1024 -> &C[MINDEX2(1024, 1024, bi * 32 "
        "+ i, j)] ~> Cell");
    for (int i = 0; i < 32; i++) {
      __strict();
      __xconsumes(
          "for j in 0..1024 -> &C[MINDEX2(1024, 1024, bi * 32 + i, j)] ~> "
          "Cell");
      __xproduces(
          "for bi1 in 0..32 -> for i2 in 0..32 -> &C[MINDEX2(1024, 1024, bi * "
          "32 + i, bi1 * 32 + i2)] ~> Cell");
      __ghost(tile_divides,
              "tile_count := 32, tile_size := 32, size := 1024, items := fun j "
              "-> &C[MINDEX2(1024, 1024, bi * 32 + i, j)] ~> Cell");
    }
    __ghost(swap_groups,
            "outer_range := 0..32, inner_range := 0..32, items := fun i, bj -> "
            "for j in 0..32 -> &C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + "
            "j)] ~> Cell");
    for (int bj = 0; bj < 32; bj++) {
      __strict();
      __sreads("A ~> Matrix2(1024, 1024)");
      __xwrites(
          "for i in 0..32 -> for j in 0..32 -> &C[MINDEX2(1024, 1024, bi * 32 "
          "+ i, bj * 32 + j)] ~> Cell");
      __xreads(
          "for bk in 0..256 -> for k in 0..4 -> for j in 0..32 -> "
          "&pB[MINDEX4(32, 256, 4, 32, bj, bk, k, j)] ~> Cell");
      float* const sum = MALLOC2(float, 32, 32);
      for (int i = 0; i < 32; i++) {
        __strict();
        __xwrites("for _v5 in 0..32 -> &sum[MINDEX2(32, 32, i, _v5)] ~> Cell");
        for (int j = 0; j < 32; j++) {
          __strict();
          __xwrites("&sum[MINDEX2(32, 32, i, j)] ~> Cell");
          sum[MINDEX2(32, 32, i, j)] = 0.f;
        }
      }
      for (int bk = 0; bk < 256; bk++) {
        __strict();
        __smodifies("sum ~> Matrix2(32, 32)");
        __sreads("A ~> Matrix2(1024, 1024)");
        __xreads(
            "for k in 0..4 -> for j in 0..32 -> &pB[MINDEX4(32, 256, 4, 32, "
            "bj, bk, k, j)] ~> Cell");
        for (int i = 0; i < 32; i++) {
          __strict();
          __sreads(
              "for k in 0..4 -> for j in 0..32 -> &pB[MINDEX4(32, 256, 4, 32, "
              "bj, bk, k, j)] ~> Cell");
          __sreads("A ~> Matrix2(1024, 1024)");
          __xmodifies("for j in 0..32 -> &sum[MINDEX2(32, 32, i, j)] ~> Cell");
          for (int j = 0; j < 32; j++) {
            __strict();
          }
          __ghost(tiled_index_in_range,
                  "tile_index := bi, index := i, tile_count := 32, tile_size "
                  ":= 32, size := 1024");
          for (int k = 0; k < 4; k++) {
            __strict();
            __smodifies(
                "for j in 0..32 -> &sum[MINDEX2(32, 32, i, j)] ~> Cell");
            __sreads("A ~> Matrix2(1024, 1024)");
            __xreads(
                "for j in 0..32 -> &pB[MINDEX4(32, 256, 4, 32, bj, bk, k, j)] "
                "~> Cell");
            for (int j = 0; j < 32; j++) {
              __strict();
              __sreads("A ~> Matrix2(1024, 1024)");
              __xmodifies("&sum[MINDEX2(32, 32, i, j)] ~> Cell");
              __xreads("&pB[MINDEX4(32, 256, 4, 32, bj, bk, k, j)] ~> Cell");
              __ghost(tiled_index_in_range,
                      "tile_index := bj, index := j, tile_count := 32, "
                      "tile_size := 32, size := 1024");
              __ghost(tiled_index_in_range,
                      "tile_index := bk, index := k, tile_count := 256, "
                      "tile_size := 4, size := 1024");
              __ghost(tiled_index_in_range,
                      "tile_index := bk, index := k, tile_count := 256, "
                      "tile_size := 4, size := 1024");
              const __ghost_fn __ghost_pair_2 =
                  __ghost_begin(ro_matrix2_focus,
                                "M := A, i := bi * 32 + i, j := bk * 4 + k");
              sum[MINDEX2(32, 32, i, j)] +=
                  A[MINDEX2(1024, 1024, bi * 32 + i, bk * 4 + k)] *
                  pB[MINDEX4(32, 256, 4, 32, bj, bk, k, j)];
              __ghost_end(__ghost_pair_2);
            }
          }
        }
      }
      for (int i = 0; i < 32; i++) {
        __strict();
        __xwrites(
            "for j in 0..32 -> &C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + "
            "j)] ~> Cell");
        __xreads("for j in 0..32 -> &sum[MINDEX2(32, 32, i, j)] ~> Cell");
        for (int j = 0; j < 32; j++) {
          __strict();
          __xwrites(
              "&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
          __xreads("&sum[MINDEX2(32, 32, i, j)] ~> Cell");
          C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] =
              sum[MINDEX2(32, 32, i, j)];
        }
      }
      free(sum);
    }
    __ghost(swap_groups,
            "outer_range := 0..32, inner_range := 0..32, items := fun bj, i -> "
            "for j in 0..32 -> &C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + "
            "j)] ~> Cell");
    for (int i = 0; i < 32; i++) {
      __strict();
      __xconsumes(
          "for bj in 0..32 -> for j in 0..32 -> &C[MINDEX2(1024, 1024, bi * 32 "
          "+ i, bj * 32 + j)] ~> Cell");
      __xproduces(
          "for j in 0..1024 -> &C[MINDEX2(1024, 1024, bi * 32 + i, j)] ~> "
          "Cell");
      __ghost(untile_divides,
              "tile_count := 32, tile_size := 32, size := 1024, items := fun j "
              "-> &C[MINDEX2(1024, 1024, bi * 32 + i, j)] ~> Cell");
    }
  }
  free(pB);
  __ghost(untile_divides,
          "tile_count := 32, tile_size := 32, size := 1024, items := fun i -> "
          "for j in 0..1024 -> &C[MINDEX2(1024, 1024, i, j)] ~> Cell");
}
