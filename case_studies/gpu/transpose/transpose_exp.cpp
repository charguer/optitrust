#include "optitrust_models.h"

#include "optitrust_gpu.h"







  void transpose (float* a, float* b, int W, int H)  {
  __requires("A: int * int -> float");
  __requires("(exact_div(W, 32) >= 0)");
  __preserves("HostCtx");
  __writes("b ~> Matrix2(W, H, fun (i: int) (j: int) -> A(j, i))");
  __reads("a ~> Matrix2(H, W, A)");
  float* const d_a = __gmem_malloc2<float>(H, W);
  __with("T := float");
  __ghost([&] ()   {
    __preserves("d_a ~> UninitMatrix2Of(H, W, GMem)");
    __admitted();
    __with("justif := shift_groups");
  });
  memcpy_host_to_device2(d_a, a, H, W);
  float* const d_b = __gmem_malloc2<float>(W, H);
  __with("T := float");
  __ghost([&] ()   {
    __preserves("d_b ~> UninitMatrix2Of(W, H, GMem)");
    __admitted();
    __with("justif := shift_groups");
  });
  __ghost(assert_prop, "P := (W = exact_div(W, 32) * 32)", "tile_div_check_x <- proof");
  __ghost(tile_divides, "div_check := tile_div_check_x, items := fun (x: int) -> for y in 0..H -> &d_b[MINDEX2(W, H, x, y)] ~> UninitCellOf(GMem)");
  /*@kernel_sequence*/{
    kernel_launch(MSIZE2(exact_div(H, 32), exact_div(W, 32)), MSIZE2(16, 32), sizeof(float) * (
      32 * 32) + 0);
    __ghost(assume, "P := (exact_div(H, 32) * (exact_div(W, 32)) = MSIZE2(exact_div(H, 32), exact_div(W, 32)))");
    __ghost(assume, "P := (MSIZE2(exact_div(H, 32), exact_div(W, 32)) * MSIZE2(16, 32) = MSIZE4(exact_div(H, 32), exact_div(W, 32), 16, 32))");
    __ghost(assume, "P := (MSIZE2(exact_div(H, 32), exact_div(W, 32)) = exact_div(H, 32) * (exact_div(W, 32)))");
    __ghost(take_smem_token, "tok_sz := sizeof(float) * (32 * 32)");
     for (int bx = 0; bx < exact_div(W, 32); bx++) {
      __strict();
      __xconsumes("for x in 0..32 -> for y in 0..H -> &d_b[MINDEX2(W, H, bx * 32 + x, y)] ~> UninitCellOf(GMem)");
      __xproduces("for j in 0..(exact_div(H, 32)) -> for i in 0..32 -> for y in 0..32 -> &d_b[MINDEX2(W, H, bx * 32 + i, j * 32 + y)] ~> UninitCellOf(GMem)");
       for (int x = 0; x < 32; x++) {
        __strict();
        __xconsumes("for y in 0..H -> &d_b[MINDEX2(W, H, bx * 32 + x, y)] ~> UninitCellOf(GMem)");
        __xproduces("for bi in 0..(exact_div(H, 32)) -> for i in 0..32 -> &d_b[MINDEX2(W, H, bx * 32 + x, bi * 32 + i)] ~> UninitCellOf(GMem)");
        __ghost(assert_prop, "P := (H = exact_div(H, 32) * 32)", "tile_div_check_y <- proof");
        __ghost(tile_divides, "div_check := tile_div_check_y, items := fun (y: int) -> &d_b[MINDEX2(W, H, bx * 32 + x, y)] ~> UninitCellOf(GMem)");
      }
      __ghost(swap_groups, "outer_range := 0..32, inner_range := 0..(exact_div(H, 32)), items := fun (x: int) (by: int) -> for y in 0..32 -> &d_b[MINDEX2(W, H, bx * 32 + x, by * 32 + y)] ~> UninitCellOf(GMem)");
    }
    __ghost(swap_groups, "outer_range := 0..(exact_div(W, 32)), inner_range := 0..(exact_div(H, 32)), items := fun (bx: int) (by: int) -> for x in 0..32 -> for y in 0..32 -> &d_b[MINDEX2(W, H, bx * 32 + x, by * 32 + y)] ~> UninitCellOf(GMem)");
    float* const tile = __smem_malloc2<float>(32, 32);
    __with("T := float");
    __ghost(rewrite_linear, "from := MSIZE2(exact_div(H, 32), exact_div(W, 32)), to := exact_div(H, 32) * (exact_div(W, 32)), inside := fun (sz: int) -> desync_for i in ..sz -> for i1 in 0..32 -> for i2 in 0..32 -> &tile[MINDEX3(sz, 32, 32, DMINDEX1(sz, i), i1, i2)] ~> UninitCellOf(SMem)");
    __ghost(desync_tile_divides, "items := fun (di: int) -> for i1 in 0..32 -> for i2 in 0..32 -> &tile[MINDEX3(exact_div(H, 32) * (exact_div(W, 32)), 32, 32, DMINDEX1(exact_div(H, 32) * (exact_div(W, 32)), di), i1, i2)] ~> UninitCellOf(SMem), div_check := eq_refl(exact_div(H, 32) * (exact_div(W, 32))), tile_count := exact_div(H, 32), tile_size := exact_div(W, 32)");
    __ghost(dmindex2_untile, "H := fun (f: int * int -> int) -> desync_for di1 in ..exact_div(H, 32) -> desync_for di2 in ..exact_div(W, 32) -> for i1 in 0..32 -> for i2 in 0..32 -> &tile[MINDEX3(exact_div(H, 32) * (exact_div(W, 32)), 32, 32, f(di1, di2), i1, i2)] ~> UninitCellOf(SMem), n1 := exact_div(H, 32), n2 := exact_div(W, 32)");
    kernel_setup_end();
    __with("grid_sz := MSIZE4(exact_div(H, 32), exact_div(W, 32), 16, 32)");
    __threadfor;  for (int by = 0; by < exact_div(H, 32); by++) {
      __strict();
      __sreads("KernelParams(MSIZE2(exact_div(H, 32), exact_div(W, 32)), MSIZE2(16, 32), sizeof(float) * (32 * 32) + 0)");
      __sreads("d_a ~> Matrix2Of(H, W, GMem, A)");
      __xconsumes("for bx in 0..(exact_div(W, 32)) -> for x in 0..32 -> for y in 0..32 -> &d_b[MINDEX2(W, H, bx * 32 + x, by * 32 + y)] ~> UninitCellOf(GMem)");
      __xproduces("desync_for bx in ..exact_div(W, 32) -> for j in 0..2 -> desync_for x in ..16 -> desync_for y in ..32 -> &d_b[MINDEX2(W, H, bx * 32 + (j * 16 + x), by * 32 + y)] ~~>[GMem] A(by * 32 + y, bx * 32 + (j * 16 + x))");
      __xwrites("desync_for bx in ..exact_div(W, 32) -> for y in 0..32 -> for x in 0..32 -> &tile[MINDEX3(exact_div(H, 32) * (exact_div(W, 32)), 32, 32, DMINDEX2(exact_div(H, 32), exact_div(W, 32), by, bx), y, x)] ~~>[SMem] A(by * 32 + y, bx * 32 + x)");
      __threadfor;  for (int bx = 0; bx < exact_div(W, 32); bx++) {
        __strict();
        __sreads("KernelParams(MSIZE2(exact_div(H, 32), exact_div(W, 32)), MSIZE2(16, 32), sizeof(float) * (32 * 32) + 0)");
        __sreads("d_a ~> Matrix2Of(H, W, GMem, A)");
        __xconsumes("for x in 0..32 -> for y in 0..32 -> &d_b[MINDEX2(W, H, bx * 32 + x, by * 32 + y)] ~> UninitCellOf(GMem)");
        __xproduces("for j in 0..2 -> desync_for x in ..16 -> desync_for y in ..32 -> &d_b[MINDEX2(W, H, bx * 32 + (j * 16 + x), by * 32 + y)] ~~>[GMem] A(by * 32 + y, bx * 32 + (j * 16 + x))");
        __xwrites("for y in 0..32 -> for x in 0..32 -> &tile[MINDEX3(exact_div(H, 32) * (exact_div(W, 32)), 32, 32, DMINDEX2(exact_div(H, 32), exact_div(W, 32), by, bx), y, x)] ~~>[SMem] A(by * 32 + y, bx * 32 + x)");
        __ghost(swap_groups, "outer_range := 0..32, inner_range := 0..32, items := fun (x: int) (y: int) -> &d_b[MINDEX2(W, H, bx * 32 + x, by * 32 + y)] ~> UninitCellOf(GMem)");
        __ghost(assert_prop, "P := (32 = 2 * 16)", "tile_div_check_y <- proof");
        __ghost(tile_divides, "div_check := tile_div_check_y, items := fun (y: int) -> for x in 0..32 -> &tile[MINDEX3(exact_div(H, 32) * (exact_div(W, 32)), 32, 32, DMINDEX2(exact_div(H, 32), exact_div(W, 32), by, bx), y, x)] ~> UninitCellOf(SMem)");
         for (int j = 0; j < 2; j++) {
          __strict();
          __spreserves("ThreadsCtx(MINDEX3(exact_div(H, 32), exact_div(W, 32), 0, by, bx, 0)..+MSIZE2(16, 32))");
          __sreads("d_a ~> Matrix2Of(H, W, GMem, A)");
          __xconsumes("for y in 0..16 -> for x in 0..32 -> &tile[MINDEX3(exact_div(H, 32) * (exact_div(W, 32)), 32, 32, DMINDEX2(exact_div(H, 32), exact_div(W, 32), by, bx), j * 16 + y, x)] ~> UninitCellOf(SMem)");
          __xproduces("desync_for y in ..16 -> desync_for x in ..32 -> &tile[MINDEX3(exact_div(H, 32) * (exact_div(W, 32)), 32, 32, DMINDEX2(exact_div(H, 32), exact_div(W, 32), by, bx), j * 16 + y, x)] ~~>[SMem] A(by * 32 + (j * 16 + y), bx * 32 + x)");
          __threadfor;  for (int y = 0; y < 16; y++) {
            __strict();
            __sreads("d_a ~> Matrix2Of(H, W, GMem, A)");
            __xconsumes("for x in 0..32 -> &tile[MINDEX3(exact_div(H, 32) * (exact_div(W, 32)), 32, 32, DMINDEX2(exact_div(H, 32), exact_div(W, 32), by, bx), j * 16 + y, x)] ~> UninitCellOf(SMem)");
            __xproduces("desync_for x in ..32 -> &tile[MINDEX3(exact_div(H, 32) * (exact_div(W, 32)), 32, 32, DMINDEX2(exact_div(H, 32), exact_div(W, 32), by, bx), j * 16 + y, x)] ~~>[SMem] A(by * 32 + (j * 16 + y), bx * 32 + x)");
            __ghost(tiled_index_in_range, "tile_index := j, index := y, div_check := tile_div_check_y");
            __threadfor;  for (int x = 0; x < 32; x++) {
              __strict();
              __sreads("d_a ~> Matrix2Of(H, W, GMem, A)");
              __xwrites("&tile[MINDEX3(exact_div(H, 32) * (exact_div(W, 32)), 32, 32, DMINDEX2(exact_div(H, 32), exact_div(W, 32), by, bx), j * 16 + y, x)] ~~>[SMem] A(by * 32 + (j * 16 + y), bx * 32 + x)");
              __ghost(assert_prop, "P := (H = exact_div(H, 32) * 32)", "tile_div_check_y13 <- proof");
              __ghost(tiled_index_in_range, "tile_index := by, index := j * 16 + y, div_check := tile_div_check_y13");
              __ghost(tiled_index_in_range, "tile_index := bx, index := x, div_check := tile_div_check_x");
              const __ghost_fn __ghost_pair_2 = __ghost_begin(ro_matrix2_focus, "matrix := d_a, i := by * 32 + (j * 16 + y), j := bx * 32 + x");
              __smem_set(&tile[MINDEX3(exact_div(H, 32) * (exact_div(W, 32)), 32, 32, DMINDEX2(exact_div(H, 32), exact_div(W, 32), by, bx), j * 16 + y, x)], __gmem_get(&d_a[MINDEX2(H, W, by * 32 + (
                j * 16 + y), bx * 32 + x)]));
              __ghost_end(__ghost_pair_2);
            }
          }
        }
        __barrier_sequence; {
          blocksync();
          __with("H := for j in 0..2 -> desync_for y in ..16 -> desync_for x in ..32 -> &tile[MINDEX3(exact_div(H, 32) * (exact_div(W, 32)), 32, 32, DMINDEX2(exact_div(H, 32), exact_div(W, 32), by, bx), j * 16 + y, x)] ~~>[SMem] A(by * 32 + (j * 16 + y), bx * 32 + x)");
        }
        __ghost(untile_divides, "div_check := tile_div_check_y, items := fun (y: int) -> for x in 0..32 -> &tile[MINDEX3(exact_div(H, 32) * (exact_div(W, 32)), 32, 32, DMINDEX2(exact_div(H, 32), exact_div(W, 32), by, bx), y, x)] ~~>[SMem] A(by * 32 + y, bx * 32 + x)");
        __ghost(swap_groups, "outer_range := 0..32, inner_range := 0..32, items := fun (y: int) (x: int) -> &d_b[MINDEX2(W, H, bx * 32 + x, by * 32 + y)] ~> UninitCellOf(GMem)");
        __ghost(assert_prop, "P := (32 = 2 * 16)", "tile_div_check_x11 <- proof");
        __ghost(tile_divides, "div_check := tile_div_check_x11, items := fun (x: int) -> for y in 0..32 -> &d_b[MINDEX2(W, H, bx * 32 + x, by * 32 + y)] ~> UninitCellOf(GMem)");
         for (int j = 0; j < 2; j++) {
          __strict();
          __spreserves("ThreadsCtx(MINDEX3(exact_div(H, 32), exact_div(W, 32), 0, by, bx, 0)..+MSIZE2(16, 32))");
          __sreads("for y in 0..32 -> for x in 0..32 -> &tile[MINDEX3(exact_div(H, 32) * (exact_div(W, 32)), 32, 32, DMINDEX2(exact_div(H, 32), exact_div(W, 32), by, bx), y, x)] ~~>[SMem] A(by * 32 + y, bx * 32 + x)");
          __xconsumes("for x in 0..16 -> for y in 0..32 -> &d_b[MINDEX2(W, H, bx * 32 + (j * 16 + x), by * 32 + y)] ~> UninitCellOf(GMem)");
          __xproduces("desync_for x in ..16 -> desync_for y in ..32 -> &d_b[MINDEX2(W, H, bx * 32 + (j * 16 + x), by * 32 + y)] ~~>[GMem] A(by * 32 + y, bx * 32 + (j * 16 + x))");
          __threadfor;  for (int x = 0; x < 16; x++) {
            __strict();
            __sreads("for y in 0..32 -> for x in 0..32 -> &tile[MINDEX3(exact_div(H, 32) * (exact_div(W, 32)), 32, 32, DMINDEX2(exact_div(H, 32), exact_div(W, 32), by, bx), y, x)] ~~>[SMem] A(by * 32 + y, bx * 32 + x)");
            __xconsumes("for y in 0..32 -> &d_b[MINDEX2(W, H, bx * 32 + (j * 16 + x), by * 32 + y)] ~> UninitCellOf(GMem)");
            __xproduces("desync_for y in ..32 -> &d_b[MINDEX2(W, H, bx * 32 + (j * 16 + x), by * 32 + y)] ~~>[GMem] A(by * 32 + y, bx * 32 + (j * 16 + x))");
            __ghost(tiled_index_in_range, "tile_index := j, index := x, div_check := tile_div_check_x11");
            __threadfor;  for (int y = 0; y < 32; y++) {
              __strict();
              __sreads("for y in 0..32 -> for x in 0..32 -> &tile[MINDEX3(exact_div(H, 32) * (exact_div(W, 32)), 32, 32, DMINDEX2(exact_div(H, 32), exact_div(W, 32), by, bx), y, x)] ~~>[SMem] A(by * 32 + y, bx * 32 + x)");
              __xwrites("&d_b[MINDEX2(W, H, bx * 32 + (j * 16 + x), by * 32 + y)] ~~>[GMem] A(by * 32 + y, bx * 32 + (j * 16 + x))");
              const __ghost_fn __ghost_pair_5 = __ghost_begin(ro_group_focus, "i := y, items := fun (y: int) -> for x in 0..32 -> &tile[MINDEX3(exact_div(H, 32) * (exact_div(W, 32)), 32, 32, DMINDEX2(exact_div(H, 32), exact_div(W, 32), by, bx), y, x)] ~~>[SMem] A(by * 32 + y, bx * 32 + x)");
              const __ghost_fn __ghost_pair_4 = __ghost_begin(ro_group_focus, "i := j * 16 + x, items := fun (x: int) -> &tile[MINDEX3(exact_div(H, 32) * (exact_div(W, 32)), 32, 32, DMINDEX2(exact_div(H, 32), exact_div(W, 32), by, bx), y, x)] ~~>[SMem] A(by * 32 + y, bx * 32 + x)");
              __ghost(assert_prop, "P := (H = exact_div(H, 32) * 32)", "tile_div_check_y135 <- proof");
              __ghost(tiled_index_in_range, "tile_index := by, index := y, div_check := tile_div_check_y135");
              __ghost(tiled_index_in_range, "tile_index := bx, index := j * 16 + x, div_check := tile_div_check_x");
              __gmem_set(&d_b[MINDEX2(W, H, bx * 32 + (j * 16 + x), by * 32 + y)], __smem_get(&tile[MINDEX3(exact_div(H, 32) * (
                exact_div(W, 32)), 32, 32, DMINDEX2(exact_div(H, 32), exact_div(W, 32), by, bx), y, j * 16 + x)]));
              __ghost_end(__ghost_pair_4);
              __ghost_end(__ghost_pair_5);
            }
          }
        }
      }
    }
    kernel_teardown_begin();
    __with("grid_sz := MSIZE4(exact_div(H, 32), exact_div(W, 32), 16, 32)");
    __barrier_sequence; {
      __ghost(kernel_teardown_sync, "H := desync_for by in ..exact_div(H, 32) -> desync_for bx in ..exact_div(W, 32) -> for y in 0..32 -> for x in 0..32 -> &tile[MINDEX3(exact_div(H, 32) * (exact_div(W, 32)), 32, 32, DMINDEX2(exact_div(H, 32), exact_div(W, 32), by, bx), y, x)] ~~>[SMem] A(by * 32 + y, bx * 32 + x)");
      __ghost(kernel_teardown_sync, "H := desync_for by in ..exact_div(H, 32) -> desync_for bx in ..exact_div(W, 32) -> for j in 0..2 -> desync_for x in ..16 -> desync_for y in ..32 -> &d_b[MINDEX2(W, H, bx * 32 + (j * 16 + x), by * 32 + y)] ~~>[GMem] A(by * 32 + y, bx * 32 + (j * 16 + x))");
    }
     for (int by = 0; by < exact_div(H, 32); by++) {
      __strict();
      __xconsumes("for bx in 0..(exact_div(W, 32)) -> for j in 0..2 -> for x in 0..16 -> for y in 0..32 -> &d_b[MINDEX2(W, H, bx * 32 + (j * 16 + x), by * 32 + y)] ~~>[GMem] A(by * 32 + y, bx * 32 + (j * 16 + x))");
      __xproduces("for bx in 0..(exact_div(W, 32)) -> for x in 0..32 -> for y in 0..32 -> &d_b[MINDEX2(W, H, bx * 32 + x, by * 32 + y)] ~~>[GMem] A(by * 32 + y, bx * 32 + x)");
       for (int bx = 0; bx < exact_div(W, 32); bx++) {
        __strict();
        __xconsumes("for j in 0..2 -> for x in 0..16 -> for y in 0..32 -> &d_b[MINDEX2(W, H, bx * 32 + (j * 16 + x), by * 32 + y)] ~~>[GMem] A(by * 32 + y, bx * 32 + (j * 16 + x))");
        __xproduces("for x in 0..32 -> for y in 0..32 -> &d_b[MINDEX2(W, H, bx * 32 + x, by * 32 + y)] ~~>[GMem] A(by * 32 + y, bx * 32 + x)");
        __ghost(assert_prop, "P := (32 = 2 * 16)", "tile_div_check_y12 <- proof");
        __ghost(assert_prop, "P := (32 = 2 * 16)", "tile_div_check_x1113 <- proof");
        __ghost(untile_divides, "div_check := tile_div_check_x1113, items := fun (x: int) -> for y in 0..32 -> &d_b[MINDEX2(W, H, bx * 32 + x, by * 32 + y)] ~~>[GMem] A(by * 32 + y, bx * 32 + x)");
        __ghost(swap_groups, "outer_range := 0..32, inner_range := 0..32, items := fun (x: int) (y: int) -> &d_b[MINDEX2(W, H, bx * 32 + x, by * 32 + y)] ~~>[GMem] A(by * 32 + y, bx * 32 + x)");
        __ghost(swap_groups, "outer_range := 0..32, inner_range := 0..32, items := fun (y: int) (x: int) -> &d_b[MINDEX2(W, H, bx * 32 + x, by * 32 + y)] ~~>[GMem] A(by * 32 + y, bx * 32 + x)");
      }
    }
    __ghost(dmindex2_tile, "H := fun (f: int * int -> int) -> for di1 in 0..(exact_div(H, 32)) -> for di2 in 0..(exact_div(W, 32)) -> for i1 in 0..32 -> for i2 in 0..32 -> &tile[MINDEX3(exact_div(H, 32) * (exact_div(W, 32)), 32, 32, f(di1, di2), i1, i2)] ~> UninitCellOf(SMem), n1 := exact_div(H, 32), n2 := exact_div(W, 32)");
    __ghost(untile_divides, "items := fun (di: int) -> for i1 in 0..32 -> for i2 in 0..32 -> &tile[MINDEX3(exact_div(H, 32) * (exact_div(W, 32)), 32, 32, DMINDEX1(exact_div(H, 32) * (exact_div(W, 32)), di), i1, i2)] ~> UninitCellOf(SMem), div_check := eq_refl(exact_div(H, 32) * (exact_div(W, 32))), tile_count := exact_div(H, 32), tile_size := exact_div(W, 32)");
    __ghost(rewrite_linear, "from := exact_div(H, 32) * (exact_div(W, 32)), to := MSIZE2(exact_div(H, 32), exact_div(W, 32)), inside := fun (sz: int) -> for i in 0..sz -> for i1 in 0..32 -> for i2 in 0..32 -> &tile[MINDEX3(sz, 32, 32, DMINDEX1(sz, i), i1, i2)] ~> UninitCellOf(SMem)");
    __smem_free2(tile, 32, 32);
    __ghost(swap_groups, "outer_range := 0..(exact_div(H, 32)), inner_range := 0..(exact_div(W, 32)), items := fun (by: int) (bx: int) -> for x in 0..32 -> for y in 0..32 -> &d_b[MINDEX2(W, H, bx * 32 + x, by * 32 + y)] ~~>[GMem] A(by * 32 + y, bx * 32 + x)");
     for (int bx = 0; bx < exact_div(W, 32); bx++) {
      __strict();
      __xconsumes("for by in 0..(exact_div(H, 32)) -> for x in 0..32 -> for y in 0..32 -> &d_b[MINDEX2(W, H, bx * 32 + x, by * 32 + y)] ~~>[GMem] A(by * 32 + y, bx * 32 + x)");
      __xproduces("for x in 0..32 -> for y in 0..H -> &d_b[MINDEX2(W, H, bx * 32 + x, y)] ~~>[GMem] A(y, bx * 32 + x)");
      __ghost(swap_groups, "outer_range := 0..(exact_div(H, 32)), inner_range := 0..32, items := fun (by: int) (x: int) -> for y in 0..32 -> &d_b[MINDEX2(W, H, bx * 32 + x, by * 32 + y)] ~~>[GMem] A(by * 32 + y, bx * 32 + x)");
       for (int x = 0; x < 32; x++) {
        __strict();
        __xconsumes("for by in 0..(exact_div(H, 32)) -> for y in 0..32 -> &d_b[MINDEX2(W, H, bx * 32 + x, by * 32 + y)] ~~>[GMem] A(by * 32 + y, bx * 32 + x)");
        __xproduces("for y in 0..H -> &d_b[MINDEX2(W, H, bx * 32 + x, y)] ~~>[GMem] A(y, bx * 32 + x)");
        __ghost(assert_prop, "P := (H = exact_div(H, 32) * 32)", "tile_div_check_y12 <- proof");
        __ghost(untile_divides, "div_check := tile_div_check_y12, items := fun (y: int) -> &d_b[MINDEX2(W, H, bx * 32 + x, y)] ~~>[GMem] A(y, bx * 32 + x)");
      }
    }
    __ghost(give_smem_token, "tok_sz := sizeof(float) * (32 * 32)");
    kernel_kill();
  }/*kernel_sequence@*/
  __ghost(untile_divides, "div_check := tile_div_check_x, items := fun (x: int) -> for y in 0..H -> &d_b[MINDEX2(W, H, x, y)] ~~>[GMem] A(y, x)");
  memcpy_device_to_host2(b, d_b, W, H);
  __ghost([&] ()   {
    __preserves("d_b ~> UninitMatrix2Of(W, H, GMem)");
    __admitted();
    __with("justif := shift_groups");
  });
  gmem_free(d_b);
  __ghost([&] ()   {
    __preserves("d_a ~> UninitMatrix2Of(H, W, GMem)");
    __admitted();
    __with("justif := shift_groups");
  });
  gmem_free(d_a);
}


