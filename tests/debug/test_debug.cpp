#include <optitrust_models.h>

__ghost_ret eq_add() {
  __requires("i: int");
  __requires("k: int");
  __requires("ipk: int");
  __requires("ipk = i + k");
  __ensures("eq: ipk = i + k");
}

void rowSum(int w, int* s, int* d, int n, int cn) {
  __requires("S: int * int -> int");
  __requires("w >= 0, n >= 1, cn >= 0");
  __writes(
      "d ~> Matrix2(n, cn, fun (i: int) (c: int) -> reduce_int_sum(w, fun k -> "
      "S(i + k, c)))");
  __reads("s ~> Matrix2(n + w - 1, cn, S)");
  for (int i = 0; i < n; i++) {
    __strict();
    __sreads("s ~> Matrix2(n + w - 1, cn, S)");
    __xwrites(
        "for c in 0..cn -> &d[MINDEX2(n, cn, i, c)] ~~> reduce_int_sum(w, fun "
        "k -> S(i + k, c))");
    __ghost(assume, "P := is_subrange(i..(w + i), 0..(n + w - 1))");
    for (int c = 0; c < cn; c++) /*
    ERROR: Postcondition check error: Linear resource not found:
d[MINDEX2(n, cn, i, c)] ~~> reduce_int_sum(w, fun k -> S(i + k, c))
In context:
_RO(#284 / range_count(0..cn), for i1 in 0..(n + w - 1) -> for i2 in 0..cn ->
s[MINDEX2(n + w - 1, cn, i1, i2)] ~~> S(i1, i2)) d[MINDEX2(n, cn, i, c)] ~~>
#9300
    */
    {
      __strict();
      __sreads("s ~> Matrix2(n + w - 1, cn, S)");
      __xwrites(
          "&d[MINDEX2(n, cn, i, c)] ~~> reduce_int_sum(w, fun k -> S(i + k, "
          "c))");
      d[MINDEX2(n, cn, i, c)] = reduce_spe1(i, w + i, s, n + w - 1, cn, c);
    }
  }
}
