#include <optitrust.h>

void f(int n, int t[3][4], int* c) {
  __requires("H: Hprop");
  __modifies("t, c, H");
  t[*c][1]++;
}
