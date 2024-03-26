#include <optitrust.h>

void consts() {
  int x;
  for (int i = 0; i < 5; i++) {
    for (int j = 0; j < 5; j++) {
      x += i + j;
    }
  }
}

void from_zero(int n, int m) {
  int x;
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < m; j++) {
      x += i + j;
    }
  }
}

void incr(int ai, int bi, int aj, int bj) {
  int x;
  for (int i = ai; i < bi; i++) {
    for (int j = aj; j < bj; j++) {
      x += i + j;
    }
  }
}
