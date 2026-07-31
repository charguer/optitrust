#include <optitrust_gpu.h>
#include <optitrust_models.h>

const int bm = 32;

const int bn = 32;

const int bk = 4;

const int tn = 4;

const int tm = 8;

__ghost(to_prove,
        "P := (MSIZE2(exact_div(32, 8), exact_div(32, 4)) = MSIZE2(4, 8))");

__ghost(to_prove, "P := (MSIZE2(4, 8) = MSIZE2(8, 4))");

__ghost(to_prove, "P := (MSIZE2(8, 4) = MSIZE2(4, 8))");

__ghost(to_prove,
        "P := (MSIZE2(4, 8) = MSIZE2(exact_div(32, 8), exact_div(32, 4)))");

__ghost(to_prove,
        "P := (MSIZE2(8, 4) = MSIZE2(exact_div(32, 8), exact_div(32, 4)))");

__ghost(to_prove,
        "P := (MSIZE2(exact_div(32, 8), exact_div(32, 4)) = MSIZE2(8, 4))");

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

void mm(float* c, float* a, float* b, int m, int n, int p) {
  __requires("A: int * int -> float");
  __requires("B: int * int -> float");
  __requires("(m >= 0)");
  __requires("(n >= 0)");
  __requires("(p >= 0)");
  __preserves("HostCtx");
  __writes("c ~> Matrix2(m, n, matmul(A, B, p))");
  __reads("a ~> Matrix2(m, p, A)");
  __reads("b ~> Matrix2(p, n, B)");
  __ghost(to_prove, "P := (exact_div(m, 32) >= 0)");
  __ghost(to_prove, "P := (exact_div(n, 32) >= 0)");
  float* const c_gmem = __gmem_malloc2<float>(m, n);
  __with("T := float");
  __ghost([&]() {
    __preserves("c_gmem ~> UninitMatrix2Of(m, n, GMem)");
    __admitted();
    __with("justif := shift_groups");
  });
  float* const a_gmem = __gmem_malloc2<float>(m, p);
  __with("T := float");
  __ghost([&]() {
    __preserves("a_gmem ~> UninitMatrix2Of(m, p, GMem)");
    __admitted();
    __with("justif := shift_groups");
  });
  memcpy_host_to_device2(a_gmem, a, m, p);
  float* const b_gmem = __gmem_malloc2<float>(p, n);
  __with("T := float");
  __ghost([&]() {
    __preserves("b_gmem ~> UninitMatrix2Of(p, n, GMem)");
    __admitted();
    __with("justif := shift_groups");
  });
  memcpy_host_to_device2(b_gmem, b, p, n);
  __ghost(assert_prop, "P := (m = exact_div(m, 32) * 32)",
          "tile_div_check_i <- proof");
  __ghost(tile_divides,
          "div_check := tile_div_check_i, items := fun (i: int) -> for j in "
          "0..n -> &c_gmem[MINDEX2(m, n, i, j)] ~> UninitCellOf(GMem)");
  {
    kernel_launch(
        MSIZE2(exact_div(m, 32), exact_div(n, 32)),
        MSIZE2(exact_div(32, 8), exact_div(32, 4)),
        sizeof(float) * (4 * 4 * 8) + (sizeof(float) * (8 * 4 * 4) + 0));
    /*@kernel_body*/ {} /*kernel_body@*/
    __ghost(take_smem_token, "tok_sz := sizeof(float) * (4 * 4 * 8)");
    __ghost(take_smem_token, "tok_sz := sizeof(float) * (8 * 4 * 4)");
    float* const a_smem = __smem_malloc3<float>(4, 4, 8);
    __with("T := float");
    __ghost(assume,
            "P := (MSIZE2(exact_div(m, 32), exact_div(n, 32)) = exact_div(m, "
            "32) * (exact_div(n, 32)))");
    __ghost(rewrite_linear,
            "from := MSIZE2(exact_div(m, 32), exact_div(n, 32)), to := "
            "exact_div(m, 32) * (exact_div(n, 32)), inside := fun (sz: int) -> "
            "desync_for i in ..sz -> for i1 in 0..4 -> for i2 in 0..4 -> for "
            "i3 in 0..8 -> &a_smem[MINDEX4(sz, 4, 4, 8, DMINDEX1(sz, i), i1, "
            "i2, i3)] ~> UninitCellOf(SMem)");
    __ghost(desync_tile_divides,
            "items := fun (di: int) -> for i1 in 0..4 -> for i2 in 0..4 -> for "
            "i3 in 0..8 -> &a_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, "
            "32)), 4, 4, 8, DMINDEX1(exact_div(m, 32) * (exact_div(n, 32)), "
            "di), i1, i2, i3)] ~> UninitCellOf(SMem), div_check := "
            "eq_refl(exact_div(m, 32) * (exact_div(n, 32))), tile_count := "
            "exact_div(m, 32), tile_size := exact_div(n, 32)");
    __ghost(
        dmindex2_untile,
        "H := fun (f: int * int -> int) -> desync_for di1 in ..exact_div(m, "
        "32) -> desync_for di2 in ..exact_div(n, 32) -> for i1 in 0..4 -> for "
        "i2 in 0..4 -> for i3 in 0..8 -> &a_smem[MINDEX4(exact_div(m, 32) * "
        "(exact_div(n, 32)), 4, 4, 8, f(di1, di2), i1, i2, i3)] ~> "
        "UninitCellOf(SMem), n1 := exact_div(m, 32), n2 := exact_div(n, 32)");
    float* const b_smem = __smem_malloc3<float>(8, 4, 4);
    __with("T := float");
    __ghost(assume,
            "P := (MSIZE2(exact_div(m, 32), exact_div(n, 32)) = exact_div(m, "
            "32) * (exact_div(n, 32)))");
    __ghost(rewrite_linear,
            "from := MSIZE2(exact_div(m, 32), exact_div(n, 32)), to := "
            "exact_div(m, 32) * (exact_div(n, 32)), inside := fun (sz: int) -> "
            "desync_for i in ..sz -> for i1 in 0..8 -> for i2 in 0..4 -> for "
            "i3 in 0..4 -> &b_smem[MINDEX4(sz, 8, 4, 4, DMINDEX1(sz, i), i1, "
            "i2, i3)] ~> UninitCellOf(SMem)");
    __ghost(desync_tile_divides,
            "items := fun (di: int) -> for i1 in 0..8 -> for i2 in 0..4 -> for "
            "i3 in 0..4 -> &b_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, "
            "32)), 8, 4, 4, DMINDEX1(exact_div(m, 32) * (exact_div(n, 32)), "
            "di), i1, i2, i3)] ~> UninitCellOf(SMem), div_check := "
            "eq_refl(exact_div(m, 32) * (exact_div(n, 32))), tile_count := "
            "exact_div(m, 32), tile_size := exact_div(n, 32)");
    __ghost(
        dmindex2_untile,
        "H := fun (f: int * int -> int) -> desync_for di1 in ..exact_div(m, "
        "32) -> desync_for di2 in ..exact_div(n, 32) -> for i1 in 0..8 -> for "
        "i2 in 0..4 -> for i3 in 0..4 -> &b_smem[MINDEX4(exact_div(m, 32) * "
        "(exact_div(n, 32)), 8, 4, 4, f(di1, di2), i1, i2, i3)] ~> "
        "UninitCellOf(SMem), n1 := exact_div(m, 32), n2 := exact_div(n, 32)");
    for (int bi = 0; bi < exact_div(m, 32); bi++) {
      __xconsumes(
          "for i in 0..32 -> for j in 0..n -> &c_gmem[MINDEX2(m, n, bi * 32 + "
          "i, j)] ~> UninitCellOf(GMem)");
      __xproduces(
          "for bj in 0..(exact_div(n, 32)) -> for ti in 0..4 -> for j in 0..8 "
          "-> for i in 0..8 -> for j68 in 0..4 -> &c_gmem[MINDEX2(m, n, bi * "
          "32 + (ti * 8 + i), bj * 32 + (j * 4 + j68))] ~> UninitCellOf(GMem)");
      __ghost(assert_prop, "P := (32 = 4 * 8)", "tile_div_check_i1 <- proof");
      {
        __ghost(tile_divides,
                "div_check := tile_div_check_i1, items := fun (i: int) -> for "
                "j in 0..n -> &c_gmem[MINDEX2(m, n, bi * 32 + i, j)] ~> "
                "UninitCellOf(GMem)");
        for (int ti = 0; ti < 4; ti++) {
          __xconsumes(
              "for i in 0..8 -> for j in 0..n -> &c_gmem[MINDEX2(m, n, bi * 32 "
              "+ (ti * 8 + i), j)] ~> UninitCellOf(GMem)");
          __xproduces(
              "for j in 0..(exact_div(n, 32)) -> for i in 0..8 -> for j32 in "
              "0..32 -> &c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i), j * 32 + "
              "j32)] ~> UninitCellOf(GMem)");
          for (int i = 0; i < 8; i++) {
            __xconsumes(
                "for j in 0..n -> &c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + "
                "i), j)] ~> UninitCellOf(GMem)");
            __xproduces(
                "for bi8 in 0..(exact_div(n, 32)) -> for i9 in 0..32 -> "
                "&c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i), bi8 * 32 + i9)] "
                "~> UninitCellOf(GMem)");
            __ghost(assert_prop, "P := (n = exact_div(n, 32) * 32)",
                    "tile_div_check_j <- proof");
            __ghost(tile_divides,
                    "div_check := tile_div_check_j, items := fun (j: int) -> "
                    "&c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i), j)] ~> "
                    "UninitCellOf(GMem)");
          }
          __ghost(swap_groups,
                  "outer_range := 0..8, inner_range := 0..(exact_div(n, 32)), "
                  "items := fun (i: int) (bj: int) -> for j in 0..32 -> "
                  "&c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i), bj * 32 + j)] "
                  "~> UninitCellOf(GMem)");
        }
        __ghost(swap_groups,
                "outer_range := 0..4, inner_range := 0..(exact_div(n, 32)), "
                "items := fun (ti: int) (bj: int) -> for i in 0..8 -> for j in "
                "0..32 -> &c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i), bj * "
                "32 + j)] ~> UninitCellOf(GMem)");
        for (int bj = 0; bj < exact_div(n, 32); bj++) {
          __xconsumes(
              "for ti in 0..4 -> for i in 0..8 -> for j in 0..32 -> "
              "&c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i), bj * 32 + j)] ~> "
              "UninitCellOf(GMem)");
          __xproduces(
              "for ti in 0..4 -> for j in 0..8 -> for i in 0..8 -> for j68 in "
              "0..4 -> &c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i), bj * 32 + "
              "(j * 4 + j68))] ~> UninitCellOf(GMem)");
          for (int ti = 0; ti < 4; ti++) {
            __xconsumes(
                "for i in 0..8 -> for j in 0..32 -> &c_gmem[MINDEX2(m, n, bi * "
                "32 + (ti * 8 + i), bj * 32 + j)] ~> UninitCellOf(GMem)");
            __xproduces(
                "for j in 0..8 -> for i in 0..8 -> for j68 in 0..4 -> "
                "&c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i), bj * 32 + (j * "
                "4 + j68))] ~> UninitCellOf(GMem)");
            {
              for (int i = 0; i < 8; i++) {
                __xconsumes(
                    "for j in 0..32 -> &c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 "
                    "+ i), bj * 32 + j)] ~> UninitCellOf(GMem)");
                __xproduces(
                    "for bi15 in 0..8 -> for i16 in 0..4 -> &c_gmem[MINDEX2(m, "
                    "n, bi * 32 + (ti * 8 + i), bj * 32 + (bi15 * 4 + i16))] "
                    "~> UninitCellOf(GMem)");
                __ghost(assert_prop, "P := (32 = 8 * 4)",
                        "tile_div_check_j2 <- proof");
                __ghost(tile_divides,
                        "div_check := tile_div_check_j2, items := fun (j: int) "
                        "-> &c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i), bj * "
                        "32 + j)] ~> UninitCellOf(GMem)");
              }
              __ghost(swap_groups,
                      "outer_range := 0..8, inner_range := 0..8, items := fun "
                      "(i: int) (tj: int) -> for j in 0..4 -> "
                      "&c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i), bj * 32 + "
                      "(tj * 4 + j))] ~> UninitCellOf(GMem)");
            }
          }
        }
      }
    }
    __ghost(assume,
            "P := (MSIZE2(exact_div(m, 32), exact_div(n, 32)) * "
            "MSIZE2(exact_div(32, 8), exact_div(32, 4)) = MSIZE4(exact_div(m, "
            "32), exact_div(n, 32), exact_div(32, 8), exact_div(32, 4)))");
    kernel_setup_end();
    __with(
        "grid_sz := MSIZE4(exact_div(m, 32), exact_div(n, 32), exact_div(32, "
        "8), exact_div(32, 4))");
    __threadfor;
    for (int bi = 0; bi < exact_div(m, 32); bi++) {
      __sreads("a_gmem ~> Matrix2Of(m, p, GMem, A)");
      __sreads("b_gmem ~> Matrix2Of(p, n, GMem, B)");
      __xpreserves(
          "desync_for _v62 in ..exact_div(n, 32) -> for _v63 in 0..8 -> for "
          "_v64 in 0..4 -> for _v65 in 0..4 -> &b_smem[MINDEX4(exact_div(m, "
          "32) * (exact_div(n, 32)), 8, 4, 4, DMINDEX2(exact_div(m, 32), "
          "exact_div(n, 32), bi, _v62), _v63, _v64, _v65)] ~> "
          "UninitCellOf(SMem)");
      __xpreserves(
          "desync_for _v55 in ..exact_div(n, 32) -> for _v56 in 0..4 -> for "
          "_v57 in 0..4 -> for _v58 in 0..8 -> &a_smem[MINDEX4(exact_div(m, "
          "32) * (exact_div(n, 32)), 4, 4, 8, DMINDEX2(exact_div(m, 32), "
          "exact_div(n, 32), bi, _v55), _v56, _v57, _v58)] ~> "
          "UninitCellOf(SMem)");
      __xwrites(
          "desync_for bj in ..exact_div(n, 32) -> desync_for ti in ..4 -> "
          "desync_for tj in ..8 -> for i in 0..8 -> for j in 0..4 -> "
          "&c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i), bj * 32 + (tj * 4 + "
          "j))] ~~>[GMem] reduce_sum(p, fun k -> A(bi * 32 + (ti * 8 + i), k) "
          "*. B(k, bj * 32 + (tj * 4 + j)))");
      __ghost(assert_prop, "P := (32 = 4 * 8)", "tile_div_check_i170 <- proof");
      __threadfor;
      for (int bj = 0; bj < exact_div(n, 32); bj++) {
        __sreads("a_gmem ~> Matrix2Of(m, p, GMem, A)");
        __sreads("b_gmem ~> Matrix2Of(p, n, GMem, B)");
        __xpreserves(
            "for _v59 in 0..8 -> for _v60 in 0..4 -> for _v61 in 0..4 -> "
            "&b_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, 32)), 8, 4, 4, "
            "DMINDEX2(exact_div(m, 32), exact_div(n, 32), bi, bj), _v59, _v60, "
            "_v61)] ~> UninitCellOf(SMem)");
        __xpreserves(
            "for _v52 in 0..4 -> for _v53 in 0..4 -> for _v54 in 0..8 -> "
            "&a_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, 32)), 4, 4, 8, "
            "DMINDEX2(exact_div(m, 32), exact_div(n, 32), bi, bj), _v52, _v53, "
            "_v54)] ~> UninitCellOf(SMem)");
        __xwrites(
            "desync_for ti in ..4 -> desync_for tj in ..8 -> for i in 0..8 -> "
            "for j in 0..4 -> &c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i), bj "
            "* 32 + (tj * 4 + j))] ~~>[GMem] reduce_sum(p, fun k -> A(bi * 32 "
            "+ (ti * 8 + i), k) *. B(k, bj * 32 + (tj * 4 + j)))");
        __ghost(rewrite_threadsctx_sz,
                "from := MSIZE2(exact_div(32, 8), exact_div(32, 4)), to := "
                "MSIZE2(4, 8)");
        float* const sum = __treg_ref_uninit2<float>(8, 4);
        __with("T := float");
        __ghost(assume, "P := (MSIZE2(4, 8) = 4 * 8)");
        __ghost(rewrite_linear,
                "from := MSIZE2(4, 8), to := 4 * 8, inside := fun (sz: int) -> "
                "desync_for i in ..sz -> for i1 in 0..8 -> for i2 in 0..4 -> "
                "&sum[MINDEX3(sz, 8, 4, DMINDEX1(sz, i), i1, i2)] ~> "
                "UninitCellOf(TReg)");
        __ghost(desync_tile_divides,
                "items := fun (di: int) -> for i1 in 0..8 -> for i2 in 0..4 -> "
                "&sum[MINDEX3(4 * 8, 8, 4, DMINDEX1(4 * 8, di), i1, i2)] ~> "
                "UninitCellOf(TReg), div_check := eq_refl(4 * 8), tile_count "
                ":= 4, tile_size := 8");
        __ghost(dmindex2_untile,
                "H := fun (f: int * int -> int) -> desync_for di1 in ..4 -> "
                "desync_for di2 in ..8 -> for i1 in 0..8 -> for i2 in 0..4 -> "
                "&sum[MINDEX3(4 * 8, 8, 4, f(di1, di2), i1, i2)] ~> "
                "UninitCellOf(TReg), n1 := 4, n2 := 8");
        __threadfor;
        for (int ti = 0; ti < 4; ti++) {
          __xwrites(
              "desync_for tj in ..8 -> for i in 0..8 -> for j in 0..4 -> "
              "&sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, 8, ti, tj), i, j)] "
              "~~>[TReg] reduce_sum(0 * 4, fun k0 -> A(bi * 32 + (ti * 8 + i), "
              "k0) *. B(k0, bj * 32 + (tj * 4 + j)))");
          __threadfor;
          for (int tj = 0; tj < 8; tj++) {
            __xwrites(
                "for i in 0..8 -> for j in 0..4 -> &sum[MINDEX3(4 * 8, 8, 4, "
                "DMINDEX2(4, 8, ti, tj), i, j)] ~~>[TReg] reduce_sum(0 * 4, "
                "fun k0 -> A(bi * 32 + (ti * 8 + i), k0) *. B(k0, bj * 32 + "
                "(tj * 4 + j)))");
            for (int i = 0; i < 8; i++) {
              __xwrites(
                  "for j in 0..4 -> &sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, 8, "
                  "ti, tj), i, j)] ~~>[TReg] reduce_sum(0 * 4, fun k0 -> A(bi "
                  "* 32 + (ti * 8 + i), k0) *. B(k0, bj * 32 + (tj * 4 + j)))");
              for (int j = 0; j < 4; j++) {
                __xwrites(
                    "&sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, 8, ti, tj), i, j)] "
                    "~~>[TReg] reduce_sum(0 * 4, fun k0 -> A(bi * 32 + (ti * 8 "
                    "+ i), k0) *. B(k0, bj * 32 + (tj * 4 + j)))");
                __treg_set(
                    &sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, 8, ti, tj), i, j)],
                    0.f);
                __ghost(rewrite_float_linear,
                        "inside := fun v -> &sum[MINDEX3(4 * 8, 8, 4, "
                        "DMINDEX2(4, 8, ti, tj), i, j)] ~~>[TReg] v, by := "
                        "reduce_sum_empty(fun k -> A(bi * 32 + (ti * 8 + i), "
                        "k) *. B(k, bj * 32 + (tj * 4 + j)))");
                __ghost(
                    rewrite_linear,
                    "inside := fun (k: int) -> &sum[MINDEX3(4 * 8, 8, 4, "
                    "DMINDEX2(4, 8, ti, tj), i, j)] ~~>[TReg] reduce_sum(k, "
                    "fun k0 -> A(bi * 32 + (ti * 8 + i), k0) *. B(k0, bj * 32 "
                    "+ (tj * 4 + j))), by := zero_mul_intro(4)");
              }
            }
          }
        }
        for (int bkIdx = 0; bkIdx < exact_div(p, 4); bkIdx++) {
          __spreserves(
              "ThreadsCtx(MINDEX3(exact_div(m, 32), exact_div(n, 32), 0, bi, "
              "bj, 0)..+MSIZE2(4, 8))");
          __spreserves(
              "for i1 in 0..8 -> for i2 in 0..4 -> for i3 in 0..4 -> "
              "&b_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, 32)), 8, 4, 4, "
              "DMINDEX2(exact_div(m, 32), exact_div(n, 32), bi, bj), i1, i2, "
              "i3)] ~> UninitCellOf(SMem)");
          __spreserves(
              "for i1 in 0..4 -> for i2 in 0..4 -> for i3 in 0..8 -> "
              "&a_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, 32)), 4, 4, 8, "
              "DMINDEX2(exact_div(m, 32), exact_div(n, 32), bi, bj), i1, i2, "
              "i3)] ~> UninitCellOf(SMem)");
          __spreserves(
              "desync_for ti in ..4 -> desync_for tj in ..8 -> for i in 0..8 "
              "-> for j in 0..4 -> &sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, 8, "
              "ti, tj), i, j)] ~~>[TReg] reduce_sum(bkIdx * 4, fun k0 -> A(bi "
              "* 32 + (ti * 8 + i), k0) *. B(k0, bj * 32 + (tj * 4 + j)))");
          __sreads("a_gmem ~> Matrix2Of(m, p, GMem, A)");
          __sreads("b_gmem ~> Matrix2Of(p, n, GMem, B)");
          __threadfor;
          for (int ti = 0; ti < 4; ti++) {
            __sreads("a_gmem ~> Matrix2Of(m, p, GMem, A)");
            __xconsumes(
                "for k in 0..4 -> for i in 0..8 -> "
                "&a_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, 32)), 4, 4, "
                "8, DMINDEX2(exact_div(m, 32), exact_div(n, 32), bi, bj), ti, "
                "k, i)] ~> UninitCellOf(SMem)");
            __xproduces(
                "for k in 0..4 -> desync_for i in ..8 -> "
                "&a_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, 32)), 4, 4, "
                "8, DMINDEX2(exact_div(m, 32), exact_div(n, 32), bi, bj), ti, "
                "k, i)] ~~>[SMem] A(bi * 32 + (ti * 8 + i), bkIdx * 4 + k)");
            for (int k = 0; k < 4; k++) {
              __spreserves(
                  "ThreadsCtx(MINDEX4(exact_div(m, 32), exact_div(n, 32), 4, "
                  "0, bi, bj, ti, 0)..+MSIZE1(8))");
              __sreads("a_gmem ~> Matrix2Of(m, p, GMem, A)");
              __xconsumes(
                  "for i in 0..8 -> &a_smem[MINDEX4(exact_div(m, 32) * "
                  "(exact_div(n, 32)), 4, 4, 8, DMINDEX2(exact_div(m, 32), "
                  "exact_div(n, 32), bi, bj), ti, k, i)] ~> "
                  "UninitCellOf(SMem)");
              __xproduces(
                  "desync_for i in ..8 -> &a_smem[MINDEX4(exact_div(m, 32) * "
                  "(exact_div(n, 32)), 4, 4, 8, DMINDEX2(exact_div(m, 32), "
                  "exact_div(n, 32), bi, bj), ti, k, i)] ~~>[SMem] A(bi * 32 + "
                  "(ti * 8 + i), bkIdx * 4 + k)");
              __threadfor;
              for (int i = 0; i < 8; i++) {
                __sreads("a_gmem ~> Matrix2Of(m, p, GMem, A)");
                __xwrites(
                    "&a_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, 32)), 4, "
                    "4, 8, DMINDEX2(exact_div(m, 32), exact_div(n, 32), bi, "
                    "bj), ti, k, i)] ~~>[SMem] A(bi * 32 + (ti * 8 + i), bkIdx "
                    "* 4 + k)");
                __ghost(assert_prop, "P := (p = exact_div(p, 4) * 4)",
                        "tile_div_check_k35 <- proof");
                __ghost(tiled_index_in_range,
                        "tile_index := bkIdx, index := k, div_check := "
                        "tile_div_check_k35");
                __ghost(tiled_index_in_range,
                        "tile_index := ti, index := i, div_check := "
                        "tile_div_check_i170");
                __ghost(tiled_index_in_range,
                        "tile_index := bi, index := ti * 8 + i, div_check := "
                        "tile_div_check_i");
                const __ghost_fn __ghost_pair_6 =
                    __ghost_begin(ro_matrix2_focus,
                                  "matrix := a_gmem, i := bi * 32 + (ti * 8 + "
                                  "i), j := bkIdx * 4 + k");
                __smem_set(
                    &a_smem[MINDEX4(
                        exact_div(m, 32) * (exact_div(n, 32)), 4, 4, 8,
                        DMINDEX2(exact_div(m, 32), exact_div(n, 32), bi, bj),
                        ti, k, i)],
                    __gmem_get(&a_gmem[MINDEX2(m, p, bi * 32 + (ti * 8 + i),
                                               bkIdx * 4 + k)]));
                __ghost_end(__ghost_pair_6);
              }
            }
          }
          __ghost(rewrite_threadsctx_sz,
                  "from := MSIZE2(4, 8), to := MSIZE2(8, 4)");
          __threadfor;
          for (int tj = 0; tj < 8; tj++) {
            __sreads("b_gmem ~> Matrix2Of(p, n, GMem, B)");
            __xconsumes(
                "for k in 0..4 -> for j in 0..4 -> "
                "&b_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, 32)), 8, 4, "
                "4, DMINDEX2(exact_div(m, 32), exact_div(n, 32), bi, bj), tj, "
                "k, j)] ~> UninitCellOf(SMem)");
            __xproduces(
                "desync_for k in ..4 -> for j in 0..4 -> "
                "&b_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, 32)), 8, 4, "
                "4, DMINDEX2(exact_div(m, 32), exact_div(n, 32), bi, bj), tj, "
                "k, j)] ~~>[SMem] B(bkIdx * 4 + k, bj * 32 + (tj * 4 + j))");
            __threadfor;
            for (int k = 0; k < 4; k++) {
              __sreads("b_gmem ~> Matrix2Of(p, n, GMem, B)");
              __xwrites(
                  "for j in 0..4 -> &b_smem[MINDEX4(exact_div(m, 32) * "
                  "(exact_div(n, 32)), 8, 4, 4, DMINDEX2(exact_div(m, 32), "
                  "exact_div(n, 32), bi, bj), tj, k, j)] ~~>[SMem] B(bkIdx * 4 "
                  "+ k, bj * 32 + (tj * 4 + j))");
              __ghost(assert_prop, "P := (p = exact_div(p, 4) * 4)",
                      "tile_div_check_k3542 <- proof");
              __ghost(tiled_index_in_range,
                      "tile_index := bkIdx, index := k, div_check := "
                      "tile_div_check_k3542");
              __ghost(assert_prop, "P := (n = exact_div(n, 32) * 32)",
                      "tile_div_check_j713222644 <- proof");
              __ghost(assert_prop, "P := (32 = 8 * 4)",
                      "tile_div_check_j214232743 <- proof");
              for (int j = 0; j < 4; j++) {
                __sreads("b_gmem ~> Matrix2Of(p, n, GMem, B)");
                __xwrites(
                    "&b_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, 32)), 8, "
                    "4, 4, DMINDEX2(exact_div(m, 32), exact_div(n, 32), bi, "
                    "bj), tj, k, j)] ~~>[SMem] B(bkIdx * 4 + k, bj * 32 + (tj "
                    "* 4 + j))");
                __ghost(tiled_index_in_range,
                        "tile_index := tj, index := j, div_check := "
                        "tile_div_check_j214232743");
                __ghost(tiled_index_in_range,
                        "tile_index := bj, index := tj * 4 + j, div_check := "
                        "tile_div_check_j713222644");
                const __ghost_fn __ghost_pair_9 =
                    __ghost_begin(ro_matrix2_focus,
                                  "matrix := b_gmem, i := bkIdx * 4 + k, j := "
                                  "bj * 32 + (tj * 4 + j)");
                __smem_set(
                    &b_smem[MINDEX4(
                        exact_div(m, 32) * (exact_div(n, 32)), 8, 4, 4,
                        DMINDEX2(exact_div(m, 32), exact_div(n, 32), bi, bj),
                        tj, k, j)],
                    __gmem_get(&b_gmem[MINDEX2(p, n, bkIdx * 4 + k,
                                               bj * 32 + (tj * 4 + j))]));
                __ghost_end(__ghost_pair_9);
              }
            }
          }
          __ghost(rewrite_threadsctx_sz,
                  "from := MSIZE2(8, 4), to := MSIZE2(exact_div(32, 8), "
                  "exact_div(32, 4))");
          /*@sync1*/ __barrier_sequence;
          {
            blocksync();
            __with(
                "H := desync_for tj in ..8 -> desync_for k in ..4 -> for j in "
                "0..4 -> &b_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, "
                "32)), 8, 4, 4, DMINDEX2(exact_div(m, 32), exact_div(n, 32), "
                "bi, bj), tj, k, j)] ~~>[SMem] B(bkIdx * 4 + k, bj * 32 + (tj "
                "* 4 + j))");
            blocksync();
            __with(
                "H := desync_for ti in ..4 -> for k in 0..4 -> desync_for i in "
                "..8 -> &a_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, 32)), "
                "4, 4, 8, DMINDEX2(exact_div(m, 32), exact_div(n, 32), bi, "
                "bj), ti, k, i)] ~~>[SMem] A(bi * 32 + (ti * 8 + i), bkIdx * 4 "
                "+ k)");
            blocksync();
            __with(
                "H := desync_for ti in ..4 -> desync_for tj in ..8 -> for i in "
                "0..8 -> for j in 0..4 -> &c_gmem[MINDEX2(m, n, bi * 32 + (ti "
                "* 8 + i), bj * 32 + (tj * 4 + j))] ~> UninitCellOf(GMem)");
          } /*sync1@*/
          __ghost(rewrite_threadsctx_sz,
                  "from := MSIZE2(exact_div(32, 8), exact_div(32, 4)), to := "
                  "MSIZE2(8, 4)");
          __ghost(rewrite_threadsctx_sz,
                  "from := MSIZE2(8, 4), to := MSIZE2(4, 8)");
          __threadfor;
          for (int ti = 0; ti < 4; ti++) {
            __sreads(
                "for ti in 0..4 -> for k in 0..4 -> for i in 0..8 -> "
                "&a_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, 32)), 4, 4, "
                "8, DMINDEX2(exact_div(m, 32), exact_div(n, 32), bi, bj), ti, "
                "k, i)] ~~>[SMem] A(bi * 32 + (ti * 8 + i), bkIdx * 4 + k)");
            __sreads(
                "for tj in 0..8 -> for k in 0..4 -> for j in 0..4 -> "
                "&b_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, 32)), 8, 4, "
                "4, DMINDEX2(exact_div(m, 32), exact_div(n, 32), bi, bj), tj, "
                "k, j)] ~~>[SMem] B(bkIdx * 4 + k, bj * 32 + (tj * 4 + j))");
            __xconsumes(
                "desync_for tj in ..8 -> for i in 0..8 -> for j in 0..4 -> "
                "&sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, 8, ti, tj), i, j)] "
                "~~>[TReg] reduce_sum(bkIdx * 4, fun k0 -> A(bi * 32 + (ti * 8 "
                "+ i), k0) *. B(k0, bj * 32 + (tj * 4 + j)))");
            __xproduces(
                "desync_for tj in ..8 -> for i in 0..8 -> for j in 0..4 -> "
                "&sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, 8, ti, tj), i, j)] "
                "~~>[TReg] reduce_sum((bkIdx + 1) * 4, fun k0 -> A(bi * 32 + "
                "(ti * 8 + i), k0) *. B(k0, bj * 32 + (tj * 4 + j)))");
            const __ghost_fn __ghost_pair_12 = __ghost_begin(
                ro_group_focus,
                "i := ti, items := fun (ti: int) -> for k in 0..4 -> for i in "
                "0..8 -> &a_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, "
                "32)), 4, 4, 8, DMINDEX2(exact_div(m, 32), exact_div(n, 32), "
                "bi, bj), ti, k, i)] ~~>[SMem] A(bi * 32 + (ti * 8 + i), bkIdx "
                "* 4 + k)");
            __threadfor;
            for (int tj = 0; tj < 8; tj++) {
              __sreads(
                  "for tj in 0..8 -> for k in 0..4 -> for j in 0..4 -> "
                  "&b_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, 32)), 8, "
                  "4, 4, DMINDEX2(exact_div(m, 32), exact_div(n, 32), bi, bj), "
                  "tj, k, j)] ~~>[SMem] B(bkIdx * 4 + k, bj * 32 + (tj * 4 + "
                  "j))");
              __sreads(
                  "for k in 0..4 -> for i in 0..8 -> "
                  "&a_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, 32)), 4, "
                  "4, 8, DMINDEX2(exact_div(m, 32), exact_div(n, 32), bi, bj), "
                  "ti, k, i)] ~~>[SMem] A(bi * 32 + (ti * 8 + i), bkIdx * 4 + "
                  "k)");
              __xconsumes(
                  "for i in 0..8 -> for j in 0..4 -> &sum[MINDEX3(4 * 8, 8, 4, "
                  "DMINDEX2(4, 8, ti, tj), i, j)] ~~>[TReg] reduce_sum(bkIdx * "
                  "4, fun k0 -> A(bi * 32 + (ti * 8 + i), k0) *. B(k0, bj * 32 "
                  "+ (tj * 4 + j)))");
              __xproduces(
                  "for i in 0..8 -> for j in 0..4 -> &sum[MINDEX3(4 * 8, 8, 4, "
                  "DMINDEX2(4, 8, ti, tj), i, j)] ~~>[TReg] reduce_sum((bkIdx "
                  "+ 1) * 4, fun k0 -> A(bi * 32 + (ti * 8 + i), k0) *. B(k0, "
                  "bj * 32 + (tj * 4 + j)))");
              const __ghost_fn __ghost_pair_11 = __ghost_begin(
                  ro_group_focus,
                  "i := tj, items := fun (tj: int) -> for k in 0..4 -> for j "
                  "in 0..4 -> &b_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, "
                  "32)), 8, 4, 4, DMINDEX2(exact_div(m, 32), exact_div(n, 32), "
                  "bi, bj), tj, k, j)] ~~>[SMem] B(bkIdx * 4 + k, bj * 32 + "
                  "(tj * 4 + j))");
              for (int i = 0; i < 8; i++) {
                __xconsumes(
                    "for j in 0..4 -> &sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, 8, "
                    "ti, tj), i, j)] ~~>[TReg] reduce_sum(bkIdx * 4, fun k0 -> "
                    "A(bi * 32 + (ti * 8 + i), k0) *. B(k0, bj * 32 + (tj * 4 "
                    "+ j)))");
                __xproduces(
                    "for j in 0..4 -> &sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, 8, "
                    "ti, tj), i, j)] ~~>[TReg] reduce_sum(bkIdx * 4 + 0, fun "
                    "k0 -> A(bi * 32 + (ti * 8 + i), k0) *. B(k0, bj * 32 + "
                    "(tj * 4 + j)))");
                for (int j = 0; j < 4; j++) {
                  __xconsumes(
                      "&sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, 8, ti, tj), i, "
                      "j)] ~~>[TReg] reduce_sum(bkIdx * 4, fun k0 -> A(bi * 32 "
                      "+ (ti * 8 + i), k0) *. B(k0, bj * 32 + (tj * 4 + j)))");
                  __xproduces(
                      "&sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, 8, ti, tj), i, "
                      "j)] ~~>[TReg] reduce_sum(bkIdx * 4 + 0, fun k0 -> A(bi "
                      "* 32 + (ti * 8 + i), k0) *. B(k0, bj * 32 + (tj * 4 + "
                      "j)))");
                  __ghost(
                      rewrite_linear,
                      "inside := fun (k: int) -> &sum[MINDEX3(4 * 8, 8, 4, "
                      "DMINDEX2(4, 8, ti, tj), i, j)] ~~>[TReg] reduce_sum(k, "
                      "fun k0 -> A(bi * 32 + (ti * 8 + i), k0) *. B(k0, bj * "
                      "32 + (tj * 4 + j))), by := plus_zero_intro(bkIdx * 4)");
                }
              }
              for (int k = 0; k < 4; k++) {
                __spreserves(
                    "for i in 0..8 -> for j in 0..4 -> &sum[MINDEX3(4 * 8, 8, "
                    "4, DMINDEX2(4, 8, ti, tj), i, j)] ~~>[TReg] "
                    "reduce_sum(bkIdx * 4 + k, fun k0 -> A(bi * 32 + (ti * 8 + "
                    "i), k0) *. B(k0, bj * 32 + (tj * 4 + j)))");
                __xreads(
                    "for i in 0..8 -> &a_smem[MINDEX4(exact_div(m, 32) * "
                    "(exact_div(n, 32)), 4, 4, 8, DMINDEX2(exact_div(m, 32), "
                    "exact_div(n, 32), bi, bj), ti, k, i)] ~~>[SMem] A(bi * 32 "
                    "+ (ti * 8 + i), bkIdx * 4 + k)");
                __xreads(
                    "for j in 0..4 -> &b_smem[MINDEX4(exact_div(m, 32) * "
                    "(exact_div(n, 32)), 8, 4, 4, DMINDEX2(exact_div(m, 32), "
                    "exact_div(n, 32), bi, bj), tj, k, j)] ~~>[SMem] B(bkIdx * "
                    "4 + k, bj * 32 + (tj * 4 + j))");
                __ghost(assert_prop, "P := (p = exact_div(p, 4) * 4)",
                        "tile_div_check_k354251 <- proof");
                __ghost(tiled_index_in_range,
                        "tile_index := bkIdx, index := k, div_check := "
                        "tile_div_check_k354251");
                __ghost(assert_prop, "P := (n = exact_div(n, 32) * 32)",
                        "tile_div_check_j71322264450 <- proof");
                __ghost(assert_prop, "P := (32 = 8 * 4)",
                        "tile_div_check_j21423274349 <- proof");
                float* const a_regs = __treg_ref_uninit1_s<float>(8);
                __with("T := float");
                for (int i = 0; i < 8; i++) {
                  __xwrites(
                      "&a_regs[MINDEX1(8, i)] ~~>[TReg] A(bi * 32 + (ti * 8 + "
                      "i), bkIdx * 4 + k)");
                  __xreads(
                      "&a_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, 32)), "
                      "4, 4, 8, DMINDEX2(exact_div(m, 32), exact_div(n, 32), "
                      "bi, bj), ti, k, i)] ~~>[SMem] A(bi * 32 + (ti * 8 + i), "
                      "bkIdx * 4 + k)");
                  __treg_set(
                      &a_regs[MINDEX1(8, i)],
                      __smem_get(&a_smem[MINDEX4(
                          exact_div(m, 32) * (exact_div(n, 32)), 4, 4, 8,
                          DMINDEX2(exact_div(m, 32), exact_div(n, 32), bi, bj),
                          ti, k, i)]));
                }
                float* const b_regs = __treg_ref_uninit1_s<float>(4);
                __with("T := float");
                for (int j = 0; j < 4; j++) {
                  __xwrites(
                      "&b_regs[MINDEX1(4, j)] ~~>[TReg] B(bkIdx * 4 + k, bj * "
                      "32 + (tj * 4 + j))");
                  __xreads(
                      "&b_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, 32)), "
                      "8, 4, 4, DMINDEX2(exact_div(m, 32), exact_div(n, 32), "
                      "bi, bj), tj, k, j)] ~~>[SMem] B(bkIdx * 4 + k, bj * 32 "
                      "+ (tj * 4 + j))");
                  __treg_set(
                      &b_regs[MINDEX1(4, j)],
                      __smem_get(&b_smem[MINDEX4(
                          exact_div(m, 32) * (exact_div(n, 32)), 8, 4, 4,
                          DMINDEX2(exact_div(m, 32), exact_div(n, 32), bi, bj),
                          tj, k, j)]));
                }
                for (int i = 0; i < 8; i++) {
                  __sreads(
                      "for j in 0..4 -> &b_regs[MINDEX1(4, j)] ~~>[TReg] "
                      "B(bkIdx * 4 + k, bj * 32 + (tj * 4 + j))");
                  __xconsumes(
                      "for j in 0..4 -> &sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, "
                      "8, ti, tj), i, j)] ~~>[TReg] reduce_sum(bkIdx * 4 + k, "
                      "fun k0 -> A(bi * 32 + (ti * 8 + i), k0) *. B(k0, bj * "
                      "32 + (tj * 4 + j)))");
                  __xproduces(
                      "for j in 0..4 -> &sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, "
                      "8, ti, tj), i, j)] ~~>[TReg] reduce_sum(bkIdx * 4 + (k "
                      "+ 1), fun k0 -> A(bi * 32 + (ti * 8 + i), k0) *. B(k0, "
                      "bj * 32 + (tj * 4 + j)))");
                  __xreads(
                      "&a_regs[MINDEX1(8, i)] ~~>[TReg] A(bi * 32 + (ti * 8 + "
                      "i), bkIdx * 4 + k)");
                  for (int j = 0; j < 4; j++) {
                    __sreads(
                        "&a_regs[MINDEX1(8, i)] ~~>[TReg] A(bi * 32 + (ti * 8 "
                        "+ i), bkIdx * 4 + k)");
                    __xconsumes(
                        "&sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, 8, ti, tj), i, "
                        "j)] ~~>[TReg] reduce_sum(bkIdx * 4 + k, fun k0 -> "
                        "A(bi * 32 + (ti * 8 + i), k0) *. B(k0, bj * 32 + (tj "
                        "* 4 + j)))");
                    __xproduces(
                        "&sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, 8, ti, tj), i, "
                        "j)] ~~>[TReg] reduce_sum(bkIdx * 4 + (k + 1), fun k0 "
                        "-> A(bi * 32 + (ti * 8 + i), k0) *. B(k0, bj * 32 + "
                        "(tj * 4 + j)))");
                    __xreads(
                        "&b_regs[MINDEX1(4, j)] ~~>[TReg] B(bkIdx * 4 + k, bj "
                        "* 32 + (tj * 4 + j))");
                    __treg_set(
                        &sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, 8, ti, tj), i,
                                     j)],
                        __treg_get(&sum[MINDEX3(
                            4 * 8, 8, 4, DMINDEX2(4, 8, ti, tj), i, j)]) +
                            __treg_get(&a_regs[MINDEX1(8, i)]) *
                                __treg_get(&b_regs[MINDEX1(4, j)]));
                    __ghost(in_range_bounds, "x := bkIdx * 4 + k",
                            "k_ge_04866 <- lower_bound, #_3567 <- upper_bound");
                    __ghost(rewrite_float_linear,
                            "inside := fun v -> &sum[MINDEX3(4 * 8, 8, 4, "
                            "DMINDEX2(4, 8, ti, tj), i, j)] ~~>[TReg] v, by := "
                            "reduce_sum_add_right(bkIdx * 4 + k, fun k -> A(bi "
                            "* 32 + (ti * 8 + i), k) *. B(k, bj * 32 + (tj * 4 "
                            "+ j)), k_ge_04866)");
                    __ghost(rewrite_linear,
                            "inside := fun (k: int) -> &sum[MINDEX3(4 * 8, 8, "
                            "4, DMINDEX2(4, 8, ti, tj), i, j)] ~~>[TReg] "
                            "reduce_sum(k, fun k0 -> A(bi * 32 + (ti * 8 + i), "
                            "k0) *. B(k0, bj * 32 + (tj * 4 + j))), by := "
                            "add_assoc_right(bkIdx * 4, k, 1)");
                  }
                }
              }
              for (int i = 0; i < 8; i++) {
                __xconsumes(
                    "for j in 0..4 -> &sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, 8, "
                    "ti, tj), i, j)] ~~>[TReg] reduce_sum(bkIdx * 4 + 4, fun "
                    "k0 -> A(bi * 32 + (ti * 8 + i), k0) *. B(k0, bj * 32 + "
                    "(tj * 4 + j)))");
                __xproduces(
                    "for j in 0..4 -> &sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, 8, "
                    "ti, tj), i, j)] ~~>[TReg] reduce_sum((bkIdx + 1) * 4, fun "
                    "k0 -> A(bi * 32 + (ti * 8 + i), k0) *. B(k0, bj * 32 + "
                    "(tj * 4 + j)))");
                for (int j = 0; j < 4; j++) {
                  __xconsumes(
                      "&sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, 8, ti, tj), i, "
                      "j)] ~~>[TReg] reduce_sum(bkIdx * 4 + 4, fun k0 -> A(bi "
                      "* 32 + (ti * 8 + i), k0) *. B(k0, bj * 32 + (tj * 4 + "
                      "j)))");
                  __xproduces(
                      "&sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, 8, ti, tj), i, "
                      "j)] ~~>[TReg] reduce_sum((bkIdx + 1) * 4, fun k0 -> "
                      "A(bi * 32 + (ti * 8 + i), k0) *. B(k0, bj * 32 + (tj * "
                      "4 + j)))");
                  __ghost(
                      rewrite_linear,
                      "inside := fun (k: int) -> &sum[MINDEX3(4 * 8, 8, 4, "
                      "DMINDEX2(4, 8, ti, tj), i, j)] ~~>[TReg] reduce_sum(k, "
                      "fun k0 -> A(bi * 32 + (ti * 8 + i), k0) *. B(k0, bj * "
                      "32 + (tj * 4 + j))), by := mul_add_factor(bkIdx, 4)");
                }
              }
              __ghost_end(__ghost_pair_11);
            }
            __ghost_end(__ghost_pair_12);
          }
        }
        __threadfor;
        for (int ti = 0; ti < 4; ti++) {
          __xconsumes(
              "desync_for tj in ..8 -> for i in 0..8 -> for j in 0..4 -> "
              "&sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, 8, ti, tj), i, j)] "
              "~~>[TReg] reduce_sum(exact_div(p, 4) * 4, fun k0 -> A(bi * 32 + "
              "(ti * 8 + i), k0) *. B(k0, bj * 32 + (tj * 4 + j)))");
          __xproduces(
              "desync_for tj in ..8 -> for _v30 in 0..8 -> for _v31 in 0..4 -> "
              "&sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, 8, ti, tj), _v30, _v31)] "
              "~> UninitCellOf(TReg)");
          __xwrites(
              "desync_for tj in ..8 -> for i in 0..8 -> for j in 0..4 -> "
              "&c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i), bj * 32 + (tj * 4 "
              "+ j))] ~~>[GMem] reduce_sum(p, fun k -> A(bi * 32 + (ti * 8 + "
              "i), k) *. B(k, bj * 32 + (tj * 4 + j)))");
          __threadfor;
          for (int tj = 0; tj < 8; tj++) {
            __xconsumes(
                "for i in 0..8 -> for j in 0..4 -> &sum[MINDEX3(4 * 8, 8, 4, "
                "DMINDEX2(4, 8, ti, tj), i, j)] ~~>[TReg] "
                "reduce_sum(exact_div(p, 4) * 4, fun k0 -> A(bi * 32 + (ti * 8 "
                "+ i), k0) *. B(k0, bj * 32 + (tj * 4 + j)))");
            __xproduces(
                "for _v30 in 0..8 -> for _v31 in 0..4 -> &sum[MINDEX3(4 * 8, "
                "8, 4, DMINDEX2(4, 8, ti, tj), _v30, _v31)] ~> "
                "UninitCellOf(TReg)");
            __xwrites(
                "for i in 0..8 -> for j in 0..4 -> &c_gmem[MINDEX2(m, n, bi * "
                "32 + (ti * 8 + i), bj * 32 + (tj * 4 + j))] ~~>[GMem] "
                "matmul(A, B, p)(bi * 32 + (ti * 8 + i), bj * 32 + (tj * 4 + "
                "j))");
            for (int i = 0; i < 8; i++) {
              __xconsumes(
                  "for j in 0..4 -> &sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, 8, "
                  "ti, tj), i, j)] ~~>[TReg] reduce_sum(exact_div(p, 4) * 4, "
                  "fun k0 -> A(bi * 32 + (ti * 8 + i), k0) *. B(k0, bj * 32 + "
                  "(tj * 4 + j)))");
              __xproduces(
                  "for _v21 in 0..4 -> &sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, "
                  "8, ti, tj), i, _v21)] ~> UninitCellOf(TReg)");
              __xwrites(
                  "for j in 0..4 -> &c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + "
                  "i), bj * 32 + (tj * 4 + j))] ~~>[GMem] matmul(A, B, p)(bi * "
                  "32 + (ti * 8 + i), bj * 32 + (tj * 4 + j))");
              for (int j = 0; j < 4; j++) {
                __xconsumes(
                    "&sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, 8, ti, tj), i, j)] "
                    "~~>[TReg] reduce_sum(exact_div(p, 4) * 4, fun k0 -> A(bi "
                    "* 32 + (ti * 8 + i), k0) *. B(k0, bj * 32 + (tj * 4 + "
                    "j)))");
                __xproduces(
                    "&sum[MINDEX3(4 * 8, 8, 4, DMINDEX2(4, 8, ti, tj), i, j)] "
                    "~> UninitCellOf(TReg)");
                __xwrites(
                    "&c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i), bj * 32 + "
                    "(tj * 4 + j))] ~~>[GMem] matmul(A, B, p)(bi * 32 + (ti * "
                    "8 + i), bj * 32 + (tj * 4 + j))");
                __ghost(assert_prop, "P := (p = exact_div(p, 4) * 4)",
                        "tile_div_check_k34 <- proof");
                __ghost(rewrite_linear,
                        "inside := fun (k: int) -> &sum[MINDEX3(4 * 8, 8, 4, "
                        "DMINDEX2(4, 8, ti, tj), i, j)] ~~>[TReg] "
                        "reduce_sum(k, fun k0 -> A(bi * 32 + (ti * 8 + i), k0) "
                        "*. B(k0, bj * 32 + (tj * 4 + j))), by := eq_sym(p, "
                        "exact_div(p, 4) * 4, tile_div_check_k34)");
                __gmem_set(&c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i),
                                           bj * 32 + (tj * 4 + j))],
                           __treg_get(&sum[MINDEX3(
                               4 * 8, 8, 4, DMINDEX2(4, 8, ti, tj), i, j)]));
              }
            }
          }
        }
        __ghost(assume, "P := (4 * 8 = MSIZE2(4, 8))");
        __ghost(dmindex2_tile,
                "H := fun (f: int * int -> int) -> desync_for di1 in ..4 -> "
                "desync_for di2 in ..8 -> for i1 in 0..8 -> for i2 in 0..4 -> "
                "&sum[MINDEX3(4 * 8, 8, 4, f(di1, di2), i1, i2)] ~> "
                "UninitCellOf(TReg), n1 := 4, n2 := 8");
        __ghost(desync_untile_divides,
                "items := fun (di: int) -> for i1 in 0..8 -> for i2 in 0..4 -> "
                "&sum[MINDEX3(4 * 8, 8, 4, DMINDEX1(4 * 8, di), i1, i2)] ~> "
                "UninitCellOf(TReg), div_check := eq_refl(4 * 8), tile_count "
                ":= 4, tile_size := 8");
        __ghost(rewrite_linear,
                "from := 4 * 8, to := MSIZE2(4, 8), inside := fun (sz: int) -> "
                "desync_for i in ..sz -> for i1 in 0..8 -> for i2 in 0..4 -> "
                "&sum[MINDEX3(sz, 8, 4, DMINDEX1(sz, i), i1, i2)] ~> "
                "UninitCellOf(TReg)");
        __ghost(rewrite_threadsctx_sz,
                "from := MSIZE2(4, 8), to := MSIZE2(exact_div(32, 8), "
                "exact_div(32, 4))");
      }
    }
    kernel_teardown_begin();
    __with(
        "grid_sz := MSIZE4(exact_div(m, 32), exact_div(n, 32), exact_div(32, "
        "8), exact_div(32, 4))");
    __barrier_sequence;
    {
      __ghost(
          kernel_teardown_sync,
          "H := desync_for bi in ..exact_div(m, 32) -> desync_for bj in "
          "..exact_div(n, 32) -> for _v59 in 0..8 -> for _v60 in 0..4 -> for "
          "_v61 in 0..4 -> &b_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, "
          "32)), 8, 4, 4, DMINDEX2(exact_div(m, 32), exact_div(n, 32), bi, "
          "bj), _v59, _v60, _v61)] ~> UninitCellOf(SMem)");
      __ghost(
          kernel_teardown_sync,
          "H := desync_for bi in ..exact_div(m, 32) -> desync_for bj in "
          "..exact_div(n, 32) -> for _v52 in 0..4 -> for _v53 in 0..4 -> for "
          "_v54 in 0..8 -> &a_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, "
          "32)), 4, 4, 8, DMINDEX2(exact_div(m, 32), exact_div(n, 32), bi, "
          "bj), _v52, _v53, _v54)] ~> UninitCellOf(SMem)");
      __ghost(kernel_teardown_sync,
              "H := desync_for bi in ..exact_div(m, 32) -> desync_for bj in "
              "..exact_div(n, 32) -> desync_for ti in ..4 -> desync_for tj in "
              "..8 -> for i in 0..8 -> for j in 0..4 -> &c_gmem[MINDEX2(m, n, "
              "bi * 32 + (ti * 8 + i), bj * 32 + (tj * 4 + j))] ~~>[GMem] "
              "reduce_sum(p, fun k -> A(bi * 32 + (ti * 8 + i), k) *. B(k, bj "
              "* 32 + (tj * 4 + j)))");
    }
    for (int bi = 0; bi < exact_div(m, 32); bi++) {
      __xconsumes(
          "for bj in 0..(exact_div(n, 32)) -> for ti in 0..4 -> for tj in 0..8 "
          "-> for i in 0..8 -> for j in 0..4 -> &c_gmem[MINDEX2(m, n, bi * 32 "
          "+ (ti * 8 + i), bj * 32 + (tj * 4 + j))] ~~>[GMem] reduce_sum(p, "
          "fun k -> A(bi * 32 + (ti * 8 + i), k) *. B(k, bj * 32 + (tj * 4 + "
          "j)))");
      __xproduces(
          "for i in 0..32 -> for j in 0..n -> &c_gmem[MINDEX2(m, n, bi * 32 + "
          "i, j)] ~~>[GMem] matmul(A, B, p)(bi * 32 + i, j)");
      __ghost(assert_prop, "P := (32 = 4 * 8)",
              "tile_div_check_i17071 <- proof");
      {
        for (int bj = 0; bj < exact_div(n, 32); bj++) {
          __xconsumes(
              "for ti in 0..4 -> for tj in 0..8 -> for i in 0..8 -> for j in "
              "0..4 -> &c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i), bj * 32 + "
              "(tj * 4 + j))] ~~>[GMem] reduce_sum(p, fun k -> A(bi * 32 + (ti "
              "* 8 + i), k) *. B(k, bj * 32 + (tj * 4 + j)))");
          __xproduces(
              "for ti in 0..4 -> for i in 0..8 -> for j in 0..32 -> "
              "&c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i), bj * 32 + j)] "
              "~~>[GMem] matmul(A, B, p)(bi * 32 + (ti * 8 + i), bj * 32 + j)");
          for (int ti = 0; ti < 4; ti++) {
            __xconsumes(
                "for tj in 0..8 -> for i in 0..8 -> for j in 0..4 -> "
                "&c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i), bj * 32 + (tj * "
                "4 + j))] ~~>[GMem] reduce_sum(p, fun k -> A(bi * 32 + (ti * 8 "
                "+ i), k) *. B(k, bj * 32 + (tj * 4 + j)))");
            __xproduces(
                "for i in 0..8 -> for j in 0..32 -> &c_gmem[MINDEX2(m, n, bi * "
                "32 + (ti * 8 + i), bj * 32 + j)] ~~>[GMem] matmul(A, B, p)(bi "
                "* 32 + (ti * 8 + i), bj * 32 + j)");
            {
              __ghost(swap_groups,
                      "outer_range := 0..8, inner_range := 0..8, items := fun "
                      "(tj: int) (i: int) -> for j in 0..4 -> "
                      "&c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i), bj * 32 + "
                      "(tj * 4 + j))] ~~>[GMem] matmul(A, B, p)(bi * 32 + (ti "
                      "* 8 + i), bj * 32 + (tj * 4 + j))");
              for (int i = 0; i < 8; i++) {
                __xconsumes(
                    "for tj in 0..8 -> for j in 0..4 -> &c_gmem[MINDEX2(m, n, "
                    "bi * 32 + (ti * 8 + i), bj * 32 + (tj * 4 + j))] "
                    "~~>[GMem] reduce_sum(p, fun k -> A(bi * 32 + (ti * 8 + "
                    "i), k) *. B(k, bj * 32 + (tj * 4 + j)))");
                __xproduces(
                    "for j in 0..32 -> &c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 "
                    "+ i), bj * 32 + j)] ~~>[GMem] matmul(A, B, p)(bi * 32 + "
                    "(ti * 8 + i), bj * 32 + j)");
                __ghost(assert_prop, "P := (32 = 8 * 4)",
                        "tile_div_check_j21419 <- proof");
                __ghost(untile_divides,
                        "div_check := tile_div_check_j21419, items := fun (j: "
                        "int) -> &c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i), "
                        "bj * 32 + j)] ~~>[GMem] matmul(A, B, p)(bi * 32 + (ti "
                        "* 8 + i), bj * 32 + j)");
              }
            }
          }
        }
        __ghost(
            swap_groups,
            "outer_range := 0..(exact_div(n, 32)), inner_range := 0..4, items "
            ":= fun (bj: int) (ti: int) -> for i in 0..8 -> for j in 0..32 -> "
            "&c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i), bj * 32 + j)] "
            "~~>[GMem] matmul(A, B, p)(bi * 32 + (ti * 8 + i), bj * 32 + j)");
        for (int ti = 0; ti < 4; ti++) {
          __xconsumes(
              "for bj in 0..(exact_div(n, 32)) -> for i in 0..8 -> for j in "
              "0..32 -> &c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i), bj * 32 "
              "+ j)] ~~>[GMem] reduce_sum(p, fun k -> A(bi * 32 + (ti * 8 + "
              "i), k) *. B(k, bj * 32 + j))");
          __xproduces(
              "for i in 0..8 -> for j in 0..n -> &c_gmem[MINDEX2(m, n, bi * 32 "
              "+ (ti * 8 + i), j)] ~~>[GMem] matmul(A, B, p)(bi * 32 + (ti * 8 "
              "+ i), j)");
          __ghost(
              swap_groups,
              "outer_range := 0..(exact_div(n, 32)), inner_range := 0..8, "
              "items := fun (bj: int) (i: int) -> for j in 0..32 -> "
              "&c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i), bj * 32 + j)] "
              "~~>[GMem] matmul(A, B, p)(bi * 32 + (ti * 8 + i), bj * 32 + j)");
          for (int i = 0; i < 8; i++) {
            __xconsumes(
                "for bj in 0..(exact_div(n, 32)) -> for j in 0..32 -> "
                "&c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i), bj * 32 + j)] "
                "~~>[GMem] reduce_sum(p, fun k -> A(bi * 32 + (ti * 8 + i), k) "
                "*. B(k, bj * 32 + j))");
            __xproduces(
                "for j in 0..n -> &c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + "
                "i), j)] ~~>[GMem] matmul(A, B, p)(bi * 32 + (ti * 8 + i), j)");
            __ghost(assert_prop, "P := (n = exact_div(n, 32) * 32)",
                    "tile_div_check_j712 <- proof");
            __ghost(untile_divides,
                    "div_check := tile_div_check_j712, items := fun (j: int) "
                    "-> &c_gmem[MINDEX2(m, n, bi * 32 + (ti * 8 + i), j)] "
                    "~~>[GMem] matmul(A, B, p)(bi * 32 + (ti * 8 + i), j)");
          }
        }
        __ghost(untile_divides,
                "div_check := tile_div_check_i17071, items := fun (i: int) -> "
                "for j in 0..n -> &c_gmem[MINDEX2(m, n, bi * 32 + i, j)] "
                "~~>[GMem] matmul(A, B, p)(bi * 32 + i, j)");
      }
    }
    __ghost(assume,
            "P := (exact_div(m, 32) * (exact_div(n, 32)) = MSIZE2(exact_div(m, "
            "32), exact_div(n, 32)))");
    __ghost(
        dmindex2_tile,
        "H := fun (f: int * int -> int) -> desync_for di1 in ..exact_div(m, "
        "32) -> desync_for di2 in ..exact_div(n, 32) -> for i1 in 0..8 -> for "
        "i2 in 0..4 -> for i3 in 0..4 -> &b_smem[MINDEX4(exact_div(m, 32) * "
        "(exact_div(n, 32)), 8, 4, 4, f(di1, di2), i1, i2, i3)] ~> "
        "UninitCellOf(SMem), n1 := exact_div(m, 32), n2 := exact_div(n, 32)");
    __ghost(desync_untile_divides,
            "items := fun (di: int) -> for i1 in 0..8 -> for i2 in 0..4 -> for "
            "i3 in 0..4 -> &b_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, "
            "32)), 8, 4, 4, DMINDEX1(exact_div(m, 32) * (exact_div(n, 32)), "
            "di), i1, i2, i3)] ~> UninitCellOf(SMem), div_check := "
            "eq_refl(exact_div(m, 32) * (exact_div(n, 32))), tile_count := "
            "exact_div(m, 32), tile_size := exact_div(n, 32)");
    __ghost(rewrite_linear,
            "from := exact_div(m, 32) * (exact_div(n, 32)), to := "
            "MSIZE2(exact_div(m, 32), exact_div(n, 32)), inside := fun (sz: "
            "int) -> desync_for i in ..sz -> for i1 in 0..8 -> for i2 in 0..4 "
            "-> for i3 in 0..4 -> &b_smem[MINDEX4(sz, 8, 4, 4, DMINDEX1(sz, "
            "i), i1, i2, i3)] ~> UninitCellOf(SMem)");
    __smem_free3(b_smem, 8, 4, 4);
    __ghost(assume,
            "P := (exact_div(m, 32) * (exact_div(n, 32)) = MSIZE2(exact_div(m, "
            "32), exact_div(n, 32)))");
    __ghost(
        dmindex2_tile,
        "H := fun (f: int * int -> int) -> desync_for di1 in ..exact_div(m, "
        "32) -> desync_for di2 in ..exact_div(n, 32) -> for i1 in 0..4 -> for "
        "i2 in 0..4 -> for i3 in 0..8 -> &a_smem[MINDEX4(exact_div(m, 32) * "
        "(exact_div(n, 32)), 4, 4, 8, f(di1, di2), i1, i2, i3)] ~> "
        "UninitCellOf(SMem), n1 := exact_div(m, 32), n2 := exact_div(n, 32)");
    __ghost(desync_untile_divides,
            "items := fun (di: int) -> for i1 in 0..4 -> for i2 in 0..4 -> for "
            "i3 in 0..8 -> &a_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, "
            "32)), 4, 4, 8, DMINDEX1(exact_div(m, 32) * (exact_div(n, 32)), "
            "di), i1, i2, i3)] ~> UninitCellOf(SMem), div_check := "
            "eq_refl(exact_div(m, 32) * (exact_div(n, 32))), tile_count := "
            "exact_div(m, 32), tile_size := exact_div(n, 32)");
    __ghost(rewrite_linear,
            "from := exact_div(m, 32) * (exact_div(n, 32)), to := "
            "MSIZE2(exact_div(m, 32), exact_div(n, 32)), inside := fun (sz: "
            "int) -> desync_for i in ..sz -> for i1 in 0..4 -> for i2 in 0..4 "
            "-> for i3 in 0..8 -> &a_smem[MINDEX4(sz, 4, 4, 8, DMINDEX1(sz, "
            "i), i1, i2, i3)] ~> UninitCellOf(SMem)");
    __smem_free3(a_smem, 4, 4, 8);
    __ghost(give_smem_token, "tok_sz := sizeof(float) * (8 * 4 * 4)");
    __ghost(give_smem_token, "tok_sz := sizeof(float) * (4 * 4 * 8)");
    kernel_kill();
  }
  __ghost(
      untile_divides,
      "div_check := tile_div_check_i, items := fun (i: int) -> for j in 0..n "
      "-> &c_gmem[MINDEX2(m, n, i, j)] ~~>[GMem] matmul(A, B, p)(i, j)");
  __ghost([&]() {
    __preserves("b_gmem ~> UninitMatrix2Of(p, n, GMem)");
    __admitted();
    __with("justif := shift_groups");
  });
  gmem_free(b_gmem);
  __ghost([&]() {
    __preserves("a_gmem ~> UninitMatrix2Of(m, p, GMem)");
    __admitted();
    __with("justif := shift_groups");
  });
  gmem_free(a_gmem);
  memcpy_device_to_host2(c, c_gmem, m, n);
  __ghost([&]() {
    __preserves("c_gmem ~> UninitMatrix2Of(m, n, GMem)");
    __admitted();
    __with("justif := shift_groups");
  });
  gmem_free(c_gmem);
}
