#include <optitrust_models.h>

int main() {
  __ghost(assert_prop, "P := (9 = 3 * 3)", "tile_div_check_i <- proof");
  for (int bi = 0; bi < 3; bi++) {
    __strict();
    for (int i = 0; i < 3; i++) {
      __strict();
      __ghost(tiled_index_in_range,
              "tile_index := bi, index := i, div_check := tile_div_check_i");
    }
  }
  int r;
  for (int bj = 0; bj < 10; bj += 3) {
    for (int j = bj; j < min(10, bj + 3); j++) {
    }
  }
  int s;
  for (int bk = 0; bk < 10; bk += 3) {
    for (int k = bk; k < bk + 3 && k < 10; k++) {
    }
  }
}
