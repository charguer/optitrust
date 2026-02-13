#include "optitrust_gpu.h"

void transpose(float *a, float *b, int W, int H) {
  __requires("A: int * int -> float");
  __requires("H_tile: H = H/32 * 32");
  __requires("W_tile: W = W/32 * 32");
  __requires("32 >= 0");
  __preserves("HostCtx");
  __reads("a ~> Matrix2(H, W, A)");
  __writes("b ~> Matrix2(W, H, fun (i : int) (j: int) -> A(j,i))");

  __ghost(assume, "32 >= 0");
  __ghost(assume, "32 = 16 * 2");
  // __ghost(assume, "W/32 >= 0"); // ???

  for (int x = 0; x < W; x++) {
    __xwrites("for y in 0..H -> &b[MINDEX2(W,H,x,y)] ~~> A(y,x)");
    for (int y = 0; y < H; y++) {
      __xwrites("&b[MINDEX2(W,H,x,y)] ~~> A(y,x)");
      __GHOST_BEGIN(focusA, ro_matrix2_focus, "a, y, x");
      b[MINDEX2(W,H,x,y)] = a[MINDEX2(H,W,y,x)];
      __GHOST_END(focusA);
    }
  }
}
