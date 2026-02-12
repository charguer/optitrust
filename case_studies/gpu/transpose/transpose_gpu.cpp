#include "optitrust_common.h"
#include <optitrust_models.h>
#include <optitrust_gpu.h>

void transpose(float *a, float *b, int W, int H) {
  __requires("A: int * int -> float");
  __requires("H_tile: H = H/32 * 32");
  __requires("W_tile: W = W/32 * 32");
  __requires("32 >= 0");
  __requires("W/32 >= 0");
  __preserves("HostCtx");
  __reads("a ~> Matrix2(H, W, A)");
  __writes("b ~> Matrix2(W, H, fun (i : int) (j: int) -> A(j,i))");

  __ghost(assume, "32 = 16 * 2", "tile32 <- H");

  float* const d_a = GMEM_MALLOC2(float, H, W);
  float* const d_b = GMEM_MALLOC2(float, W, H);

  memcpy_host_to_device2(d_a, a, H, W);

  const int bpg = (H/32) * (W/32); // easier to have this one as a product
  const int tpb = MSIZE2(16, 32); // and this one as MSIZE
  const int grid_sz = MSIZE4(H/32, W/32, 16, 32);
  // note: normally transfos would put everything in MSIZE and take care of rewrite ghosts, this is just to make things easier

  __DEF(b0_inside0, "fun (x y: int) -> &d_b[MINDEX2(W, H, x, y)] ~> UninitCellOf(GMem)");
  __DEF(b0_inside1, "fun (by: int) -> fun (ty x: int) -> &d_b[MINDEX2(W, H, x, by * 32 + ty)] ~> UninitCellOf(GMem)");
  __DEF(b0_inside2, "fun (by bx: int) -> fun (tx ty: int) -> &d_b[MINDEX2(W, H, bx * 32 + tx, by * 32 + ty)] ~> UninitCellOf(GMem)");
  __DEF(b0_inside3, "fun (by bx ty: int) -> fun (j tx: int) -> &d_b[MINDEX2(W, H, bx * 32 + (ty * 2 + j), by * 32 + tx)] ~> UninitCellOf(GMem)");

  __DEF(bf_inside0, "fun (x y: int) -> &d_b[MINDEX2(W, H, x, y)] ~~> [GMem] A(y,x)");
  __DEF(bf_inside1, "fun (by: int) -> fun (ty x: int) -> &d_b[MINDEX2(W, H, x, by * 32 + ty)] ~~> [GMem] A(by * 32 + ty, x)");
  __DEF(bf_inside2, "fun (by bx: int) -> fun (tx ty: int) -> &d_b[MINDEX2(W, H, bx * 32 + tx, by * 32 + ty)] ~~> [GMem] A(by * 32 + ty, bx * 32 + tx)");
  __DEF(bf_inside3, "fun (by bx ty: int) -> fun (j tx: int) -> &d_b[MINDEX2(W, H, bx * 32 + (ty * 2 + j), by * 32 + tx)] ~~> [GMem] A(by * 32 + tx, bx * 32 + (ty*2+j))");

  // TODO does not need to be an assumption
  __ghost(assume, "P := bpg * tpb = grid_sz", "thread_tile <- H");
  {
  kernel_launch(bpg, tpb, sizeof(float) * (32 * 32) + 0);

  float* const tile_grid = SMEM_MALLOC2(float, 32, 32);

  __DEF(tile_grid_inside0, "fun (bxy ty tx: int) -> &tile_grid[MINDEX3(bpg, 32, 32, DMINDEX1(bpg, bxy), ty, tx)] ~> UninitCellOf(SMem)");
  __DEF(tile_grid_inside1, "fun (by bx ty tx: int) -> &tile_grid[MINDEX3(bpg, 32, 32, DMINDEX2(H/32, W/32, by, bx), ty, tx)] ~> UninitCellOf(SMem)");
  __DEF(tile_grid_inside2, "fun (by bx ty xo xi: int) -> &tile_grid[MINDEX3(bpg, 32, 32, DMINDEX2(H/32, W/32, by, bx), ty, xo * 2 + xi)] ~> UninitCellOf(SMem)");


  __ghost(desync_tile_divides, "items := fun bxy -> for ty in 0..32 -> for tx in 0..32 -> tile_grid_inside0(bxy, ty, tx), div_check := eq_refl(bpg), tile_count := H/32, tile_size := W/32");
  __ghost(dmindex2_untile, "H := fun f -> desync_for i1 in ..H/32 -> desync_for i2 in ..W/32 -> for j1 in 0..32 -> for j2 in 0..32 -> &tile_grid[MINDEX3(bpg, 32, 32, f(i1,i2), j1, j2)] ~> UninitCellOf(SMem), n1 := H/32, n2 := W/32");

  kernel_setup_end();__with("grid_sz := grid_sz");

  __ghost(swap_groups, "items := b0_inside0");
  __ghost(tile_divides, "items := fun y -> for x in 0..W -> b0_inside0(x, y), div_check := H_tile");
  __ghost(group_to_desyncgroup, "items := fun by -> for ty in 0..32 -> for x in 0..W -> (b0_inside1(by))(ty, x)");

/*          reorder        tile                    dg outside
  | t=MX1(0,sz) sz=MSZ4(w/32, h/32, 16, 32)
  g(w, g(h)) -> g(h, g(w)) -> g(h/32, g(32, g(w))) -> dg(h/32, g(32, g(w)))
  |  t = MX2(by, 0, w/32, sz) sz=MSZ3(h/32, 16, 32)
  |  g(32h, g(w)) -> g(w, g(32h)) -> g(w/32, g(32w, g(32h))) -> dg(w/32, g(32w, g(32h)))
     |
     |  g(32w, g(32h)) -> g(32h, g(32w)) -> g(16h, g(2h, g(32w))) -> dg(16h, g(2h, g(32w)))
        |
        |  g(2h, g(32w)) -> g(32w, g(2h)) -> dg(32w, g(2h))
           |
           |  g(2h)*/

  __threadfor; for (int by = 0; by < H/32; by++) {
    __xconsumes("for ty in 0..32 -> for x in 0..W -> (b0_inside1(by))(ty, x)");
    __xproduces("desync_for bx in ..W/32 -> desync_for ty in ..16 -> desync_for tx in ..32 -> for j in 0..2 -> (bf_inside3(by,bx,ty))(j,tx)");
    __xconsumes("desync_for bx in ..W/32 -> for ty in 0..32 -> for tx in 0..32 -> tile_grid_inside1(by, bx, ty, tx)");
    __xproduces("desync_for bx in ..W/32 -> desync_for xo in ..16 -> desync_for ty in ..32 -> for xi in 0..2 -> tile_grid_inside2(by, bx, ty, xo, xi)");

    __ghost(swap_groups, "items := b0_inside1(by)");
    __ghost(tile_divides, "items := fun x -> for ty in 0..32 -> (b0_inside1(by))(ty, x), div_check := W_tile");
    __ghost(group_to_desyncgroup, "items := fun bx -> for tx in 0..32 -> for ty in 0..32 -> (b0_inside2(by,bx))(tx,ty)");

    __threadfor; for (int bx = 0; bx < W/32; bx++) {
      __xconsumes("for tx in 0..32 -> for ty in 0..32 -> (b0_inside2(by,bx))(tx,ty)");
      __xproduces("desync_for ty in ..16 -> desync_for tx in ..32 -> for j in 0..2 -> (bf_inside3(by,bx,ty))(j,tx)");
      __xconsumes("for ty in 0..32 -> for tx in 0..32 -> tile_grid_inside1(by, bx, ty, tx)");
      // Have to inline definition here otherwise uninit coercion does not work
      __xproduces("desync_for xo in ..16 -> desync_for ty in ..32 -> for xi in 0..2 -> &tile_grid[MINDEX3(bpg, 32, 32, DMINDEX2(H/32, W/32, by, bx), ty, xo * 2 + xi)] ~> UninitCellOf(SMem)");

      float* const tile = &tile_grid[MINDEX3(bpg, 32, 32, DMINDEX2(H/32, W/32, by, bx), 0, 0)];

      // Note: tx ty swap takes place here
      __ghost(tile_divides, "items := fun ty -> for tx in 0..32 -> (b0_inside2(by,bx))(ty, tx), div_check := tile32");
      __ghost(group_to_desyncgroup, "items := fun ty -> for j in 0..2 -> for tx in 0..32 -> (b0_inside3(by,bx,ty))(j,tx)");


      __DEF(tile_inside0, "fun (y x: int) -> &tile[MINDEX2(32, 32, y, x)] ~> UninitCellOf(SMem)");
      __DEF(tile_inside1, "fun (yo yi x: int) -> &tile[MINDEX2(32, 32, yo*2 + yi, x)] ~> UninitCellOf(SMem)");
      __DEF(tile_inside2, "fun (yo yi x: int) -> &tile[MINDEX2(32, 32, yo*2 + yi, x)] ~~>[SMem] A(by * 32 + (yo * 2 + yi), bx * 32 + x)");
      __DEF(tile_inside3, "fun (y x: int) -> &tile[MINDEX2(32, 32, y, x)] ~~>[SMem] A(by * 32 + y, bx * 32 + x)");
      __DEF(tile_inside4, "fun (y xo xi: int) -> &tile[MINDEX2(32, 32, y, xo*2+xi)] ~~>[SMem] A(by * 32 + y, bx * 32 + (xo*2+xi))");

      __ghost(tile_divides, "items := fun y -> for x in 0..32 -> tile_inside0(y, x), div_check := tile32");
      __ghost(group_to_desyncgroup, "items := fun yo -> for yi in 0..2 -> for x in 0..32 -> tile_inside1(yo, yi, x)");

      __threadfor; for (int ty = 0; ty < 16; ty++) {
        __xconsumes("for yi in 0..2 -> for x in 0..32 -> tile_inside1(ty, yi, x)");
        __xproduces("desync_for x in ..32 -> for yi in 0..2 -> tile_inside2(ty, yi, x)");

        __ghost(swap_groups, "items := fun (yi x: int) -> tile_inside1(ty, yi, x)");
        __ghost(group_to_desyncgroup, "items := fun x -> for j in 0..2 -> (tile_inside1(ty,j,x))");

        __threadfor; for (int tx = 0; tx < 32; tx++) {
          __xconsumes("for yi in 0..2 -> tile_inside1(ty, yi, tx)");
          __xproduces("for yi in 0..2 -> tile_inside2(ty, yi, tx)");

          // TODO: this j loop pattern is not quite the same as what the CUDA samples does?
          // the two sequential iterations should be on the outside dimension
          // should be j * 16 + ty
          for (int j = 0; j < 2; j++) {
            __xconsumes("tile_inside1(ty, j, tx)");
            __xproduces("tile_inside2(ty, j, tx)");

            const int iy = by * 32 + (ty * 2 + j);
            const int ix = bx * 32 + tx;

            // TODO assumed for now
            __ghost(assume, "P := in_range(iy, 0..H)");
            __ghost(assume, "P := in_range(ix, 0..W)");

            __GHOST_BEGIN(focusA, ro_matrix2_focus, "d_a, iy, ix");
            const float v = __gmem_get(&d_a[MINDEX2(H, W, iy, ix)]);
            __GHOST_END(focusA);
            __smem_set(&tile[MINDEX2(32,32,ty * 2 + j,tx)], v);
          }
        }
      }

      blocksync(); __with("H := desync_for ty in ..16 -> desync_for tx in ..32 -> for j in 0..2 -> tile_inside2(ty,j,tx)");

      for (int ty = 0; ty < 16; ty++) {
        __xconsumes("for tx in 0..32 -> for j in 0..2 -> tile_inside2(ty,j,tx)");
        __xproduces("for j in 0..2 -> for tx in 0..32 -> tile_inside2(ty,j,tx)");
        __ghost(swap_groups, "items := fun (tx j: int) -> tile_inside2(ty,j,tx)");
      }
      __ghost(untile_divides, "items := fun y -> for x in 0..32 -> tile_inside3(y,x), div_check := tile32");

      for (int y = 0; y < 32; y++) {
        __xconsumes("for x in 0..32 -> tile_inside3(y,x)");
        __xproduces("for xo in 0..16 -> for xi in 0..2 -> tile_inside4(y,xo,xi)");
        __ghost(tile_divides, "items := fun x -> tile_inside3(y,x), div_check := tile32");
      }

      __ghost(swap_groups, "items := fun (y xo: int) -> for xi in 0..2 -> tile_inside4(y,xo,xi)");
      __ghost(group_to_desyncgroup, "items := fun ty -> for tx in 0..32 -> for j in 0..2 -> tile_inside4(tx,ty,j)");

      __threadfor; for (int ty = 0; ty < 16; ty++) {
          __xconsumes("for tx in 0..32 -> for j in 0..2 -> tile_inside4(tx,ty,j)");
          __xconsumes("for j in 0..2 -> for tx in 0..32 -> (b0_inside3(by,bx,ty))(j,tx)");
          __xproduces("desync_for tx in ..32 -> for j in 0..2 -> tile_inside4(tx,ty,j)");
          __xproduces("desync_for tx in ..32 -> for j in 0..2 -> (bf_inside3(by,bx,ty))(j,tx)");

          __ghost(swap_groups, "items := b0_inside3(by,bx,ty)");
          __ghost(group_to_desyncgroup, "items := fun tx -> for j in 0..2 -> (b0_inside3(by,bx,ty))(j,tx)");
          __ghost(group_to_desyncgroup, "items := fun tx -> for j in 0..2 -> (tile_inside4(tx,ty,j))");

          __threadfor; for (int tx = 0; tx < 32; tx++) {
            __xpreserves("for j in 0..2 -> tile_inside4(tx,ty,j)");
            __xconsumes("for j in 0..2 -> (b0_inside3(by,bx,ty))(j,tx)");
            __xproduces("for j in 0..2 -> (bf_inside3(by,bx,ty))(j,tx)");

            for (int j = 0; j < 2; j++) {
              __xpreserves("tile_inside4(tx,ty,j)");
              __xconsumes("(b0_inside3(by,bx,ty))(j,tx)");
              __xproduces("(bf_inside3(by,bx,ty))(j,tx)");

              const float v = __smem_get(&tile[MINDEX2(32,32,tx,ty*2+j)]);
              __gmem_set(&d_b[MINDEX2(W, H, bx * 32 + (ty * 2 + j), by * 32 + tx)], v);
          }
        }
      }
    }
  }

  kernel_teardown_begin();

  __ghost(kernel_teardown_sync, "H := desync_for by in ..H/32 -> desync_for bx in ..W/32 -> desync_for ty in ..16 -> desync_for tx in ..32 -> for j in 0..2 -> (bf_inside3(by,bx,ty))(j,tx)");
  __ghost(kernel_teardown_sync, "H := desync_for by in ..H/32 -> desync_for bx in ..W/32 -> desync_for xo in ..16 -> desync_for ty in ..32 -> for xi in 0..2 -> tile_grid_inside2(by, bx, ty, xo, xi)");

  for (int by = 0; by < H / 32; by++) {
    __xconsumes("for bx in 0..(W / 32) -> for ty in 0..16 -> for tx in 0..32 -> for j in 0..2 -> (bf_inside3(by,bx,ty))(j,tx)");
    __xproduces("for ty in 0..32 -> for x in 0..W -> (bf_inside1(by))(ty,x)");
    __xconsumes("for bx in 0..W/32 -> for xo in 0..16 -> for ty in 0..32 -> for xi in 0..2 -> tile_grid_inside2(by, bx, ty, xo, xi)");
    __xproduces("for bx in 0..W/32 -> for ty in 0..32 -> for tx in 0..32 -> tile_grid_inside1(by, bx, ty, tx)");

    for (int bx = 0; bx < W / 32; bx++) {
      __xconsumes("for ty in 0..16 -> for tx in 0..32 -> for j in 0..2 -> (bf_inside3(by,bx,ty))(j,tx)");
      __xproduces("for tx in 0..32 -> for ty in 0..32 -> (bf_inside2(by,bx))(tx,ty)");
      __xconsumes("for xo in 0..16 -> for ty in 0..32 -> for xi in 0..2 -> tile_grid_inside2(by, bx, ty, xo, xi)");
      __xproduces("for ty in 0..32 -> for tx in 0..32 -> tile_grid_inside1(by, bx, ty, tx)"); // ????

      for (int ty = 0; ty < 16; ty++) {
        __xconsumes("for tx in 0..32 -> for j in 0..2 -> (bf_inside3(by,bx,ty))(j,tx)");
        __xproduces("for j in 0..2 -> for tx in 0..32 -> (bf_inside3(by,bx,ty))(j,tx)");
        __xconsumes("for tx in 0..32 -> for xi in 0..2 -> tile_grid_inside2(by, bx, tx, ty, xi)");
        __xproduces("for xi in 0..2 -> for tx in 0..32 -> tile_grid_inside2(by, bx, tx, ty, xi)");
        __ghost(swap_groups, "items := fun (tx j: int) -> bf_inside3(by,bx,ty)(j,tx)");
        __ghost(swap_groups, "items := fun (tx xi: int) -> tile_grid_inside2(by,bx,tx,ty,xi)");
      }
      __ghost(untile_divides, "items := fun ty -> for tx in 0..32 -> (bf_inside2(by,bx))(ty,tx), div_check := tile32");
      __ghost(untile_divides, "items := fun ty -> for tx in 0..32 -> (tile_grid_inside1(by,bx,tx,ty)), div_check := tile32");
      __ghost(swap_groups, "items := fun (ty tx: int) -> (tile_grid_inside1(by,bx,tx,ty))");
    }
    __ghost(untile_divides, "items := fun x -> for ty in 0..32 -> (bf_inside1(by))(ty,x), div_check := W_tile");
    __ghost(swap_groups, "items := fun (x ty: int) -> (bf_inside1(by))(ty,x)");
  }

  __ghost(untile_divides, "items := fun y -> for x in 0..W -> (bf_inside0)(x,y), div_check := H_tile");
  __ghost(swap_groups, "items := fun (y x: int) -> bf_inside0(x,y)");

  __ghost(dmindex2_tile, "H := fun f -> for i1 in 0..H/32 -> for i2 in 0..W/32 -> for j1 in 0..32 -> for j2 in 0..32 -> &tile_grid[MINDEX3(bpg, 32, 32, f(i1,i2), j1, j2)] ~> UninitCellOf(SMem), n1 := H/32, n2 := W/32");
  __ghost(untile_divides, "items := fun bxy -> for ty in 0..32 -> for tx in 0..32 -> tile_grid_inside0(bxy, ty, tx), div_check := eq_refl(bpg), tile_count := H/32, tile_size := W/32");
  __smem_free2(tile_grid, 32, 32);

  kernel_kill();
  }

  memcpy_device_to_host2(b, d_b, W, H);

  gmem_free(d_a);
  gmem_free(d_b);
}
