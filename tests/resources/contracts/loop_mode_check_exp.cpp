#include <optitrust_models.h>

void f(int* a, int* b, int N) {
  __requires("N: int");
  __consumes("for i in 0..N -> &a[MINDEX1(N, i)] ~~> 0");
  __produces("for i in 0..N -> &a[MINDEX1(N, i)] ~~> 2");
  __preserves("b ~~> 0");
  __ghost(assert_prop, "P := __is_true(N == exact_div(N, 4) * 4)",
          "tile_div_check_i <- proof");
  __ghost(tile_divides,
          "div_check := tile_div_check_i, items := fun (i: int) -> "
          "&a[MINDEX1(N, i)] ~~> 0");
  for (int ii = 0; ii < exact_div(N, 4); ii++) {
    __strict();
    __spreserves("b ~~> 0");
    __xconsumes("for i in 0..4 -> &a[MINDEX1(N, ii * 4 + i)] ~~> 0");
    __xproduces("for i in 0..4 -> &a[MINDEX1(N, ii * 4 + i)] ~~> 2");
    for (int i = 0; i < 4; i++) {
      __strict();
      __spreserves("b ~~> 0");
      __xconsumes("&a[MINDEX1(N, ii * 4 + i)] ~~> 0");
      __xproduces("&a[MINDEX1(N, ii * 4 + i)] ~~> 2");
      __ghost(tiled_index_in_range,
              "tile_index := ii, index := i, div_check := tile_div_check_i");
      a[MINDEX1(N, ii * 4 + i)] = 1;
      a[MINDEX1(N, ii * 4 + i)] = 2;
    }
  }
  __ghost(untile_divides,
          "div_check := tile_div_check_i, items := fun (i: int) -> "
          "&a[MINDEX1(N, i)] ~~> 2");
}
