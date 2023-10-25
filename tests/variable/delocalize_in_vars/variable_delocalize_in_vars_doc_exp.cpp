#include "../../../include/optitrust.h"

typedef int T;

T CHOOSE(int nb, T b1, T b2) { return b1; }

int main() {
  const int N = 2;
  T a;
  /*no-brace*/ {
    T xa;
    T xb;
    xa = a;
    xb = 0;
    for (int i = 0; i < 2; i++) {
      CHOOSE(2, xa, xb)++;
    }
    a = xa;
    a += xb;
  }
  int y = 0;
  return 0;
}
