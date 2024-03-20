#include <optitrust.h>

int main() {
__pure();
  int x = 0;
  const int st = 0;
  const int N = 10;
  for (int i2 = 0 + 2; i2 < 10 + 2; i2++) {
__strict();
__sequentially_modifies("&x ~> Cell");
    const int i = i2 - 2;
    x += i;
  }
  for (int j2 = 0; j2 < st + N - st; j2++) {
__strict();
__sequentially_modifies("&x ~> Cell");
    const const int j = j2 - -st;
    x += j;
  }
  int shift = 5;
  for (int k = 0; k < N; k++) {
    __strict();
    __sequentially_modifies("&x ~> Cell");
    x += k;
  }
  for (int m3 = 4; m3 < N - 2 + (4 - 2); m3++) {
    __strict();
    __sequentially_modifies("&x ~> Cell");
    const int m = m3 - (4 - 2);
    x += m;
  }
}

void excl_array(int* t, int n) {
  __modifies("t ~> Matrix1(n)");
  __ghost(group_shift,
          "start := 0, stop := n, step := 1, items := fun i -> &t[MINDEX1(n, "
          "i)] ~> Cell, shift := 2, new_start := 0 + 2, new_stop := n + 2");
  for (int j = 0 + 2; j < n + 2; j++) {
    __strict();
    __modifies("&t[MINDEX1(n, j - 2)] ~> Cell");
    const int i = j - 2;
    t[MINDEX1(n, i)] += i;
  }
  __ghost(group_unshift,
          "start := 0, stop := n, step := 1, items := fun i -> &t[MINDEX1(n, "
          "i)] ~> Cell, shift := 2, new_start := 0 + 2, new_stop := n + 2");
}
