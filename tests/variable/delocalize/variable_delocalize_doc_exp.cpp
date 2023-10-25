#include "../../../include/optitrust.h"

typedef int T;

int main() {
  const int N = 2;
  T a;
  /*@A*/ /*no-brace*/ {
    T x[N];
    x[0] = a;
    for (int k = 1; k < N; k++) {
      x[k] = 0;
    }
    for (int i = 0; i < N; i++) {
      x[ANY(N)]++;
    }
    a = x[0];
    for (int k = 1; k < N; k++) {
      a += x[k];
    }
  } /*A@*/
  for (int j = 0; j < N; j++) {
    a++;
  }
  return 0;
}
