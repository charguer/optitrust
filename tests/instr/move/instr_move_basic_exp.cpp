#include "../../../include/optitrust.h"

int main() {
  __pure();
  int x = 3;
  int y = 1;
  int z = 5;
  x = y;
  z = x;
  int t = 2;
  t = x;
  const int v = 0;
  y = v;
}
