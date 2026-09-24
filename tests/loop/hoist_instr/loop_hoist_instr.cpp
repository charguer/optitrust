#include <optitrust_models.h>

void f(int* t) {
  __writes("t ~> Matrix2(4, 4, fun (ij km: int) -> 0)");

  __ghost(tile_divides, "size := 4, tile_count := 2, tile_size := 2, items := fun ij -> for km in 0..4 -> &t[MINDEX2(4, 4, ij, km)] ~> UninitCell");
  for (int i = 0; i < 2; i++) {
    __xwrites("for j in 0..2 -> for km in 0..4 -> &t[MINDEX2(4, 4, i*2 + j, km)] ~~> 0");

    for (int j = 0; j < 2; j++) {
      __xwrites("for km in 0..4 -> &t[MINDEX2(4, 4, i*2 + j, km)] ~~> 0");

      __ghost(tile_divides, "size := 4, tile_count := 2, tile_size := 2, items := fun km -> &t[MINDEX2(4, 4, i*2 + j, km)] ~> UninitCell");
      for (int k = 0; k < 2; k++) {
      __xwrites("for m in 0..2 -> &t[MINDEX2(4, 4, i*2 + j, k*2 + m)] ~~> 0");
        for (int m = 0; m < 2; m++) {
        __xwrites("&t[MINDEX2(4, 4, i*2 + j, k*2 + m)] ~~> 0");
          t[MINDEX2(4, 4, i*2 + j, k*2 + m)] = 0;
        }
      }
      __ghost(untile_divides, "size := 4, tile_count := 2, tile_size := 2, items := fun km -> &t[MINDEX2(4, 4, i*2 + j, km)] ~~> 0");
    }
  }
  __ghost(untile_divides, "size := 4, tile_count := 2, tile_size := 2, items := fun ij -> for km in 0..4 -> &t[MINDEX2(4, 4, ij, km)] ~~> 0");
}
