#include <optitrust_models.h>

#include "omp.h"

__ghost(assert_inhabited, "x := arbitrary(int * (int -> float) -> float)",
        "reduce_sum <- x");

__ghost(assert_prop,
        "proof := admit(forall (f: int -> float) -> (0.f =. reduce_sum(0, f)))",
        "reduce_sum_empty <- proof");

__ghost(assert_prop,
        "proof := admit(forall (n: int) (f: int -> float) (_: (n >= 0)) -> "
        "(reduce_sum(n, f) +. f(n) =. reduce_sum(n + 1, f)))",
        "reduce_sum_add_right <- proof");

__ghost(define,
        "x := fun (A: int * int -> float) (B: int * int -> float) (p: int) -> "
        "fun (i: int) (j: int) -> reduce_sum(p, fun k -> A(i, k) *. B(k, j))",
        "matmul <- x");

void mm1024(float* c, float* a, float* b) {
  __requires("A: int * int -> float");
  __requires("B: int * int -> float");
  __writes("c ~> Matrix2(1024, 1024, matmul(A, B, 1024))");
  __reads("a ~> Matrix2(1024, 1024, A)");
  __reads("b ~> Matrix2(1024, 1024, B)");
  __ghost(assert_prop, "P := (1024 = 32 * 32)", "tile_div_check_i <- proof");
  __ghost(tile_divides,
          "div_check := tile_div_check_i, items := fun (i: int) -> for j in "
          "0..1024 -> &c[MINDEX2(1024, 1024, i, j)] ~> UninitCell");
  float* const bT = (float*)malloc(MSIZE4(32, 256, 4, 32) * sizeof(float));
#pragma omp parallel for
  for (int bj = 0; bj < 32; bj++) {
    __strict();
    __sreads("b ~> Matrix2(1024, 1024, B)");
    __xwrites(
        "for bk in 0..256 -> for k in 0..4 -> for j in 0..32 -> "
        "&bT[MINDEX4(32, 256, 4, 32, bj, bk, k, j)] ~~> B(bk * 4 + k, bj * 32 "
        "+ j)");
    for (int bk = 0; bk < 256; bk++) {
      __strict();
      __sreads("b ~> Matrix2(1024, 1024, B)");
      __xwrites(
          "for k in 0..4 -> for j in 0..32 -> &bT[MINDEX4(32, 256, 4, 32, bj, "
          "bk, k, j)] ~~> B(bk * 4 + k, bj * 32 + j)");
      __ghost(assert_prop, "P := (1024 = 32 * 32)",
              "tile_div_check_j512 <- proof");
      for (int k = 0; k < 4; k++) {
        __strict();
        __sreads("b ~> Matrix2(1024, 1024, B)");
        __xwrites(
            "for j in 0..32 -> &bT[MINDEX4(32, 256, 4, 32, bj, bk, k, j)] ~~> "
            "B(bk * 4 + k, bj * 32 + j)");
        for (int j = 0; j < 32; j++) {
          __strict();
          __sreads("b ~> Matrix2(1024, 1024, B)");
          __xwrites(
              "&bT[MINDEX4(32, 256, 4, 32, bj, bk, k, j)] ~~> B(bk * 4 + k, bj "
              "* 32 + j)");
          __ghost(assert_prop, "P := (1024 = 256 * 4)",
                  "tile_div_check_k13 <- proof");
          __ghost(
              tiled_index_in_range,
              "tile_index := bk, index := k, div_check := tile_div_check_k13",
              "");
          __ghost(
              tiled_index_in_range,
              "tile_index := bj, index := j, div_check := tile_div_check_j512",
              "");
          const __ghost_fn __ghost_pair_3 =
              __ghost_begin(ro_matrix2_focus,
                            "matrix := b, i := bk * 4 + k, j := bj * 32 + j");
          bT[MINDEX4(32, 256, 4, 32, bj, bk, k, j)] =
              b[MINDEX2(1024, 1024, bk * 4 + k, bj * 32 + j)];
          __ghost_end(__ghost_pair_3);
        }
      }
    }
  }
#pragma omp parallel for
  for (int bi = 0; bi < 32; bi++) {
    __strict();
    __sreads(
        "for bj in 0..32 -> for bk in 0..256 -> for k in 0..4 -> for j in "
        "0..32 -> &bT[MINDEX4(32, 256, 4, 32, bj, bk, k, j)] ~~> B(bk * 4 + k, "
        "bj * 32 + j)");
    __sreads("a ~> Matrix2(1024, 1024, A)");
    __xwrites(
        "for i in 0..32 -> for j in 0..1024 -> &c[MINDEX2(1024, 1024, bi * 32 "
        "+ i, j)] ~~> matmul(A, B, 1024)(bi * 32 + i, j)");
    for (int i = 0; i < 32; i++) {
      __strict();
      __xconsumes(
          "for j in 0..1024 -> &c[MINDEX2(1024, 1024, bi * 32 + i, j)] ~> "
          "UninitCell");
      __xproduces(
          "for bi6 in 0..32 -> for i7 in 0..32 -> &c[MINDEX2(1024, 1024, bi * "
          "32 + i, bi6 * 32 + i7)] ~> UninitCell");
      __ghost(assert_prop, "P := (1024 = 32 * 32)",
              "tile_div_check_j <- proof");
      __ghost(tile_divides,
              "div_check := tile_div_check_j, items := fun (j: int) -> "
              "&c[MINDEX2(1024, 1024, bi * 32 + i, j)] ~> UninitCell");
    }
    __ghost(swap_groups,
            "outer_range := 0..32, inner_range := 0..32, items := fun (i: int) "
            "(bj: int) -> for j in 0..32 -> &c[MINDEX2(1024, 1024, bi * 32 + "
            "i, bj * 32 + j)] ~> UninitCell");
    for (int bj = 0; bj < 32; bj++) {
      __strict();
      __sreads("a ~> Matrix2(1024, 1024, A)");
      __xwrites(
          "for i in 0..32 -> for j in 0..32 -> &c[MINDEX2(1024, 1024, bi * 32 "
          "+ i, bj * 32 + j)] ~~> matmul(A, B, 1024)(bi * 32 + i, bj * 32 + "
          "j)");
      __xreads(
          "for bk in 0..256 -> for k in 0..4 -> for j in 0..32 -> "
          "&bT[MINDEX4(32, 256, 4, 32, bj, bk, k, j)] ~~> B(bk * 4 + k, bj * "
          "32 + j)");
      float* const sum = (float*)malloc(MSIZE2(32, 32) * sizeof(float));
      for (int i = 0; i < 32; i++) {
        __strict();
        __xwrites(
            "for j in 0..32 -> &sum[MINDEX2(32, 32, i, j)] ~~> reduce_sum(0 * "
            "4, fun k0 -> A(bi * 32 + i, k0) *. B(k0, bj * 32 + j))");
        for (int j = 0; j < 32; j++) {
          __strict();
          __xwrites(
              "&sum[MINDEX2(32, 32, i, j)] ~~> reduce_sum(0 * 4, fun k0 -> "
              "A(bi * 32 + i, k0) *. B(k0, bj * 32 + j))");
          sum[MINDEX2(32, 32, i, j)] = 0.f;
          __ghost(rewrite_float_linear,
                  "inside := fun v -> &sum[MINDEX2(32, 32, i, j)] ~~> v, by := "
                  "reduce_sum_empty(fun k -> A(bi * 32 + i, k) *. B(k, bj * 32 "
                  "+ j))");
          __ghost(rewrite_linear,
                  "inside := fun (k: int) -> &sum[MINDEX2(32, 32, i, j)] ~~> "
                  "reduce_sum(k, fun k0 -> A(bi * 32 + i, k0) *. B(k0, bj * 32 "
                  "+ j)), by := zero_mul_intro(4)");
        }
      }
      for (int bk = 0; bk < 256; bk++) {
        __strict();
        __spreserves(
            "for i in 0..32 -> for j in 0..32 -> &sum[MINDEX2(32, 32, i, j)] "
            "~~> reduce_sum(bk * 4, fun k0 -> A(bi * 32 + i, k0) *. B(k0, bj * "
            "32 + j))");
        __sreads("a ~> Matrix2(1024, 1024, A)");
        __xreads(
            "for k in 0..4 -> for j in 0..32 -> &bT[MINDEX4(32, 256, 4, 32, "
            "bj, bk, k, j)] ~~> B(bk * 4 + k, bj * 32 + j)");
        __ghost(assert_prop, "P := (1024 = 32 * 32)",
                "tile_div_check_j51222 <- proof");
        for (int i = 0; i < 32; i++) {
          __strict();
          __sreads(
              "for k in 0..4 -> for j in 0..32 -> &bT[MINDEX4(32, 256, 4, 32, "
              "bj, bk, k, j)] ~~> B(bk * 4 + k, bj * 32 + j)");
          __sreads("a ~> Matrix2(1024, 1024, A)");
          __xconsumes(
              "for j in 0..32 -> &sum[MINDEX2(32, 32, i, j)] ~~> reduce_sum(bk "
              "* 4, fun k0 -> A(bi * 32 + i, k0) *. B(k0, bj * 32 + j))");
          __xproduces(
              "for j in 0..32 -> &sum[MINDEX2(32, 32, i, j)] ~~> "
              "reduce_sum((bk + 1) * 4, fun k0 -> A(bi * 32 + i, k0) *. B(k0, "
              "bj * 32 + j))");
          for (int j = 0; j < 32; j++) {
            __strict();
            __xconsumes(
                "&sum[MINDEX2(32, 32, i, j)] ~~> reduce_sum(bk * 4, fun k0 -> "
                "A(bi * 32 + i, k0) *. B(k0, bj * 32 + j))");
            __xproduces(
                "&sum[MINDEX2(32, 32, i, j)] ~~> reduce_sum(bk * 4 + 0, fun k0 "
                "-> A(bi * 32 + i, k0) *. B(k0, bj * 32 + j))");
            __ghost(rewrite_linear,
                    "inside := fun (k: int) -> &sum[MINDEX2(32, 32, i, j)] ~~> "
                    "reduce_sum(k, fun k0 -> A(bi * 32 + i, k0) *. B(k0, bj * "
                    "32 + j)), by := plus_zero_intro(bk * 4)");
          }
          __ghost(tiled_index_in_range,
                  "tile_index := bi, index := i, div_check := tile_div_check_i",
                  "");
          float s[MSIZE1(32)];
          const __ghost_fn __ghost_pair_6 =
              __ghost_begin(ro_mindex2_unfold,
                            "H := fun (access: int * int -> float*) -> for j "
                            "in 0..32 -> access(i, j) ~~> reduce_sum(bk * 4 + "
                            "0, fun k0 -> A(bi * 32 + i, k0) *. B(k0, bj * 32 "
                            "+ j)), matrix := sum, n1 := 32, n2 := 32");
          MATRIX1_COPY_float(s, &sum[i * 32], 32);
          __ghost_end(__ghost_pair_6);
          __ghost(assume, "P := in_range(0, 0..4)");
          __ghost(assume, "P := in_range(1, 0..4)");
          __ghost(assume, "P := in_range(2, 0..4)");
          __ghost(assume, "P := in_range(3, 0..4)");
          const __ghost_fn __ghost_pair_7 =
              __ghost_begin(ro_group_focus,
                            "i := 0, items := fun (k: int) -> for j in 0..32 "
                            "-> &bT[MINDEX4(32, 256, 4, 32, bj, bk, k, j)] ~~> "
                            "B(bk * 4 + k, bj * 32 + j)");
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            __strict();
            __sreads("a ~> Matrix2(1024, 1024, A)");
            __xconsumes(
                "&s[MINDEX1(32, j)] ~~> reduce_sum(bk * 4 + 0, fun k0 -> A(bi "
                "* 32 + i, k0) *. B(k0, bj * 32 + j))");
            __xproduces(
                "&s[MINDEX1(32, j)] ~~> reduce_sum(bk * 4 + (0 + 1), fun k0 -> "
                "A(bi * 32 + i, k0) *. B(k0, bj * 32 + j))");
            __xreads(
                "&bT[MINDEX4(32, 256, 4, 32, bj, bk, 0, j)] ~~> B(bk * 4 + 0, "
                "bj * 32 + j)");
            __ghost(tiled_index_in_range,
                    "tile_index := bj, index := j, div_check := "
                    "tile_div_check_j51222",
                    "");
            __ghost(assert_prop, "P := (1024 = 256 * 4)",
                    "tile_div_check_k1320 <- proof");
            __ghost(tiled_index_in_range,
                    "tile_index := bk, index := 0, div_check := "
                    "tile_div_check_k1320",
                    "");
            const __ghost_fn __ghost_pair_2 =
                __ghost_begin(ro_matrix2_focus,
                              "matrix := a, i := bi * 32 + i, j := bk * 4 + 0");
            s[MINDEX1(32, j)] +=
                a[MINDEX2(1024, 1024, bi * 32 + i, bk * 4 + 0)] *
                bT[MINDEX4(32, 256, 4, 32, bj, bk, 0, j)];
            __ghost_end(__ghost_pair_2);
            __ghost(in_range_bounds, "x := bk * 4 + 0",
                    "k_ge_021 <- lower_bound, #_23 <- upper_bound");
            __ghost(rewrite_float_linear,
                    "inside := fun v -> &s[MINDEX1(32, j)] ~~> v, by := "
                    "reduce_sum_add_right(bk * 4 + 0, fun k -> A(bi * 32 + i, "
                    "k) *. B(k, bj * 32 + j), k_ge_021)");
            __ghost(rewrite_linear,
                    "inside := fun (k: int) -> &s[MINDEX1(32, j)] ~~> "
                    "reduce_sum(k, fun k0 -> A(bi * 32 + i, k0) *. B(k0, bj * "
                    "32 + j)), by := add_assoc_right(bk * 4, 0, 1)");
          }
          __ghost_end(__ghost_pair_7);
          __ghost(rewrite_linear,
                  "from := 0 + 1, to := 1, inside := fun (k: int) -> for j in "
                  "0..32 -> &s[MINDEX1(32, j)] ~~> reduce_sum(bk * 4 + k, fun "
                  "k0 -> A(bi * 32 + i, k0) *. B(k0, bj * 32 + j))");
          const __ghost_fn __ghost_pair_723 =
              __ghost_begin(ro_group_focus,
                            "i := 1, items := fun (k: int) -> for j in 0..32 "
                            "-> &bT[MINDEX4(32, 256, 4, 32, bj, bk, k, j)] ~~> "
                            "B(bk * 4 + k, bj * 32 + j)");
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            __strict();
            __sreads("a ~> Matrix2(1024, 1024, A)");
            __xconsumes(
                "&s[MINDEX1(32, j)] ~~> reduce_sum(bk * 4 + 1, fun k0 -> A(bi "
                "* 32 + i, k0) *. B(k0, bj * 32 + j))");
            __xproduces(
                "&s[MINDEX1(32, j)] ~~> reduce_sum(bk * 4 + (1 + 1), fun k0 -> "
                "A(bi * 32 + i, k0) *. B(k0, bj * 32 + j))");
            __xreads(
                "&bT[MINDEX4(32, 256, 4, 32, bj, bk, 1, j)] ~~> B(bk * 4 + 1, "
                "bj * 32 + j)");
            __ghost(tiled_index_in_range,
                    "tile_index := bj, index := j, div_check := "
                    "tile_div_check_j51222",
                    "");
            __ghost(assert_prop, "P := (1024 = 256 * 4)",
                    "tile_div_check_k1320 <- proof");
            __ghost(tiled_index_in_range,
                    "tile_index := bk, index := 1, div_check := "
                    "tile_div_check_k1320",
                    "");
            const __ghost_fn __ghost_pair_2 =
                __ghost_begin(ro_matrix2_focus,
                              "matrix := a, i := bi * 32 + i, j := bk * 4 + 1");
            s[MINDEX1(32, j)] +=
                a[MINDEX2(1024, 1024, bi * 32 + i, bk * 4 + 1)] *
                bT[MINDEX4(32, 256, 4, 32, bj, bk, 1, j)];
            __ghost_end(__ghost_pair_2);
            __ghost(in_range_bounds, "x := bk * 4 + 1",
                    "k_ge_021 <- lower_bound, #_23 <- upper_bound");
            __ghost(rewrite_float_linear,
                    "inside := fun v -> &s[MINDEX1(32, j)] ~~> v, by := "
                    "reduce_sum_add_right(bk * 4 + 1, fun k -> A(bi * 32 + i, "
                    "k) *. B(k, bj * 32 + j), k_ge_021)");
            __ghost(rewrite_linear,
                    "inside := fun (k: int) -> &s[MINDEX1(32, j)] ~~> "
                    "reduce_sum(k, fun k0 -> A(bi * 32 + i, k0) *. B(k0, bj * "
                    "32 + j)), by := add_assoc_right(bk * 4, 1, 1)");
          }
          __ghost_end(__ghost_pair_723);
          __ghost(rewrite_linear,
                  "from := 1 + 1, to := 2, inside := fun (k: int) -> for j in "
                  "0..32 -> &s[MINDEX1(32, j)] ~~> reduce_sum(bk * 4 + k, fun "
                  "k0 -> A(bi * 32 + i, k0) *. B(k0, bj * 32 + j))");
          const __ghost_fn __ghost_pair_724 =
              __ghost_begin(ro_group_focus,
                            "i := 2, items := fun (k: int) -> for j in 0..32 "
                            "-> &bT[MINDEX4(32, 256, 4, 32, bj, bk, k, j)] ~~> "
                            "B(bk * 4 + k, bj * 32 + j)");
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            __strict();
            __sreads("a ~> Matrix2(1024, 1024, A)");
            __xconsumes(
                "&s[MINDEX1(32, j)] ~~> reduce_sum(bk * 4 + 2, fun k0 -> A(bi "
                "* 32 + i, k0) *. B(k0, bj * 32 + j))");
            __xproduces(
                "&s[MINDEX1(32, j)] ~~> reduce_sum(bk * 4 + (2 + 1), fun k0 -> "
                "A(bi * 32 + i, k0) *. B(k0, bj * 32 + j))");
            __xreads(
                "&bT[MINDEX4(32, 256, 4, 32, bj, bk, 2, j)] ~~> B(bk * 4 + 2, "
                "bj * 32 + j)");
            __ghost(tiled_index_in_range,
                    "tile_index := bj, index := j, div_check := "
                    "tile_div_check_j51222",
                    "");
            __ghost(assert_prop, "P := (1024 = 256 * 4)",
                    "tile_div_check_k1320 <- proof");
            __ghost(tiled_index_in_range,
                    "tile_index := bk, index := 2, div_check := "
                    "tile_div_check_k1320",
                    "");
            const __ghost_fn __ghost_pair_2 =
                __ghost_begin(ro_matrix2_focus,
                              "matrix := a, i := bi * 32 + i, j := bk * 4 + 2");
            s[MINDEX1(32, j)] +=
                a[MINDEX2(1024, 1024, bi * 32 + i, bk * 4 + 2)] *
                bT[MINDEX4(32, 256, 4, 32, bj, bk, 2, j)];
            __ghost_end(__ghost_pair_2);
            __ghost(in_range_bounds, "x := bk * 4 + 2",
                    "k_ge_021 <- lower_bound, #_23 <- upper_bound");
            __ghost(rewrite_float_linear,
                    "inside := fun v -> &s[MINDEX1(32, j)] ~~> v, by := "
                    "reduce_sum_add_right(bk * 4 + 2, fun k -> A(bi * 32 + i, "
                    "k) *. B(k, bj * 32 + j), k_ge_021)");
            __ghost(rewrite_linear,
                    "inside := fun (k: int) -> &s[MINDEX1(32, j)] ~~> "
                    "reduce_sum(k, fun k0 -> A(bi * 32 + i, k0) *. B(k0, bj * "
                    "32 + j)), by := add_assoc_right(bk * 4, 2, 1)");
          }
          __ghost_end(__ghost_pair_724);
          __ghost(rewrite_linear,
                  "from := 2 + 1, to := 3, inside := fun (k: int) -> for j in "
                  "0..32 -> &s[MINDEX1(32, j)] ~~> reduce_sum(bk * 4 + k, fun "
                  "k0 -> A(bi * 32 + i, k0) *. B(k0, bj * 32 + j))");
          const __ghost_fn __ghost_pair_725 =
              __ghost_begin(ro_group_focus,
                            "i := 3, items := fun (k: int) -> for j in 0..32 "
                            "-> &bT[MINDEX4(32, 256, 4, 32, bj, bk, k, j)] ~~> "
                            "B(bk * 4 + k, bj * 32 + j)");
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            __strict();
            __sreads("a ~> Matrix2(1024, 1024, A)");
            __xconsumes(
                "&s[MINDEX1(32, j)] ~~> reduce_sum(bk * 4 + 3, fun k0 -> A(bi "
                "* 32 + i, k0) *. B(k0, bj * 32 + j))");
            __xproduces(
                "&s[MINDEX1(32, j)] ~~> reduce_sum(bk * 4 + (3 + 1), fun k0 -> "
                "A(bi * 32 + i, k0) *. B(k0, bj * 32 + j))");
            __xreads(
                "&bT[MINDEX4(32, 256, 4, 32, bj, bk, 3, j)] ~~> B(bk * 4 + 3, "
                "bj * 32 + j)");
            __ghost(tiled_index_in_range,
                    "tile_index := bj, index := j, div_check := "
                    "tile_div_check_j51222",
                    "");
            __ghost(assert_prop, "P := (1024 = 256 * 4)",
                    "tile_div_check_k1320 <- proof");
            __ghost(tiled_index_in_range,
                    "tile_index := bk, index := 3, div_check := "
                    "tile_div_check_k1320",
                    "");
            const __ghost_fn __ghost_pair_2 =
                __ghost_begin(ro_matrix2_focus,
                              "matrix := a, i := bi * 32 + i, j := bk * 4 + 3");
            s[MINDEX1(32, j)] +=
                a[MINDEX2(1024, 1024, bi * 32 + i, bk * 4 + 3)] *
                bT[MINDEX4(32, 256, 4, 32, bj, bk, 3, j)];
            __ghost_end(__ghost_pair_2);
            __ghost(in_range_bounds, "x := bk * 4 + 3",
                    "k_ge_021 <- lower_bound, #_23 <- upper_bound");
            __ghost(rewrite_float_linear,
                    "inside := fun v -> &s[MINDEX1(32, j)] ~~> v, by := "
                    "reduce_sum_add_right(bk * 4 + 3, fun k -> A(bi * 32 + i, "
                    "k) *. B(k, bj * 32 + j), k_ge_021)");
            __ghost(rewrite_linear,
                    "inside := fun (k: int) -> &s[MINDEX1(32, j)] ~~> "
                    "reduce_sum(k, fun k0 -> A(bi * 32 + i, k0) *. B(k0, bj * "
                    "32 + j)), by := add_assoc_right(bk * 4, 3, 1)");
          }
          __ghost_end(__ghost_pair_725);
          __ghost(rewrite_linear,
                  "from := 3 + 1, to := 4, inside := fun (k: int) -> for j in "
                  "0..32 -> &s[MINDEX1(32, j)] ~~> reduce_sum(bk * 4 + k, fun "
                  "k0 -> A(bi * 32 + i, k0) *. B(k0, bj * 32 + j))");
          __ghost(
              mindex2_unfold,
              "H := fun (access: int * int -> float*) -> for j in 0..32 -> "
              "access(i, j) ~> UninitCell, matrix := sum, n1 := 32, n2 := 32");
          MATRIX1_COPY_float(&sum[i * 32], s, 32);
          __ghost(mindex2_fold,
                  "H := fun (access: int * int -> float*) -> for j in 0..32 -> "
                  "access(i, j) ~~> reduce_sum(bk * 4 + 4, fun k0 -> A(bi * 32 "
                  "+ i, k0) *. B(k0, bj * 32 + j)), matrix := sum, n1 := 32, "
                  "n2 := 32");
          for (int j = 0; j < 32; j++) {
            __strict();
            __xconsumes(
                "&sum[MINDEX2(32, 32, i, j)] ~~> reduce_sum(bk * 4 + 4, fun k0 "
                "-> A(bi * 32 + i, k0) *. B(k0, bj * 32 + j))");
            __xproduces(
                "&sum[MINDEX2(32, 32, i, j)] ~~> reduce_sum((bk + 1) * 4, fun "
                "k0 -> A(bi * 32 + i, k0) *. B(k0, bj * 32 + j))");
            __ghost(rewrite_linear,
                    "inside := fun (k: int) -> &sum[MINDEX2(32, 32, i, j)] ~~> "
                    "reduce_sum(k, fun k0 -> A(bi * 32 + i, k0) *. B(k0, bj * "
                    "32 + j)), by := mul_add_factor(bk, 4)");
          }
        }
      }
      for (int i = 0; i < 32; i++) {
        __strict();
        __xconsumes(
            "for j in 0..32 -> &sum[MINDEX2(32, 32, i, j)] ~~> reduce_sum(256 "
            "* 4, fun k0 -> A(bi * 32 + i, k0) *. B(k0, bj * 32 + j))");
        __xproduces(
            "for _v11 in 0..32 -> &sum[MINDEX2(32, 32, i, _v11)] ~> "
            "UninitCell");
        __xwrites(
            "for j in 0..32 -> &c[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + "
            "j)] ~~> matmul(A, B, 1024)(bi * 32 + i, bj * 32 + j)");
        for (int j = 0; j < 32; j++) {
          __strict();
          __xconsumes(
              "&sum[MINDEX2(32, 32, i, j)] ~~> reduce_sum(256 * 4, fun k0 -> "
              "A(bi * 32 + i, k0) *. B(k0, bj * 32 + j))");
          __xproduces("&sum[MINDEX2(32, 32, i, j)] ~> UninitCell");
          __xwrites(
              "&c[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~~> matmul(A, "
              "B, 1024)(bi * 32 + i, bj * 32 + j)");
          __ghost(assert_prop, "P := (1024 = 256 * 4)",
                  "tile_div_check_k12 <- proof");
          __ghost(rewrite_linear,
                  "inside := fun (k: int) -> &sum[MINDEX2(32, 32, i, j)] ~~> "
                  "reduce_sum(k, fun k0 -> A(bi * 32 + i, k0) *. B(k0, bj * 32 "
                  "+ j)), by := eq_sym(1024, 256 * 4, tile_div_check_k12)");
          c[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] =
              sum[MINDEX2(32, 32, i, j)];
        }
      }
      free(sum);
    }
    __ghost(
        swap_groups,
        "outer_range := 0..32, inner_range := 0..32, items := fun (bj: int) "
        "(i: int) -> for j in 0..32 -> &c[MINDEX2(1024, 1024, bi * 32 + i, bj "
        "* 32 + j)] ~~> matmul(A, B, 1024)(bi * 32 + i, bj * 32 + j)");
    for (int i = 0; i < 32; i++) {
      __strict();
      __xconsumes(
          "for bj in 0..32 -> for j in 0..32 -> &c[MINDEX2(1024, 1024, bi * 32 "
          "+ i, bj * 32 + j)] ~~> reduce_sum(1024, fun k -> A(bi * 32 + i, k) "
          "*. B(k, bj * 32 + j))");
      __xproduces(
          "for j in 0..1024 -> &c[MINDEX2(1024, 1024, bi * 32 + i, j)] ~~> "
          "matmul(A, B, 1024)(bi * 32 + i, j)");
      __ghost(assert_prop, "P := (1024 = 32 * 32)",
              "tile_div_check_j510 <- proof");
      __ghost(untile_divides,
              "div_check := tile_div_check_j510, items := fun (j: int) -> "
              "&c[MINDEX2(1024, 1024, bi * 32 + i, j)] ~~> matmul(A, B, "
              "1024)(bi * 32 + i, j)");
    }
  }
  free(bT);
  __ghost(
      untile_divides,
      "div_check := tile_div_check_i, items := fun (i: int) -> for j in "
      "0..1024 -> &c[MINDEX2(1024, 1024, i, j)] ~~> matmul(A, B, 1024)(i, j)");
}
