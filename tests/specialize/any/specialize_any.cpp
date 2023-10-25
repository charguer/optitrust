#include "../../../include/optitrust.h"

int const N = 2;

int main() {
  int corners[4];
  corners[ANY(4)] = 9;
  int a;
  int x[N];
  x[0] = a;
  for (int k = 1; k < N; k++) {
    x[k] = 0;
  }
  for (int i = 0; i < N; i++) {
    x[ANY(N)]++;
  }
  return 0;
}
