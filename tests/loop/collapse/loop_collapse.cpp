#include <optitrust.h>

void consts() {
  __pure();

  int x;
  for (int i = 0; i < 5; i++) {
    for (int j = 0; j < 5; j++) {
      x += i + j;
    }
  }
}

void from_zero(int n, int m) {
  // __pure();
  __requires("n >= 0, m >= 0");

  int x;
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < m; j++) {
      x += i + j;
    }
  }
}

void from_zero_wrong(int n, int m) {
  // __pure();
  __requires("n >= 0, m >= 0");

  int x;
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < m; j++) {
      x += i + j;
    }
  }
}
