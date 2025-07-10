#include <optitrust.h>
int f(int N1, int N2, int N3, int i1, int i2, int i3) {
  int const block_size = 10;
  float * const a = MALLOC3(float, N1, N2, N3);
  a[MINDEX3(N1, N2, N3, i1, i2, i3)] = 0;
}
// Partir avec 3 dim, pas de chiffres, mettre des variables
