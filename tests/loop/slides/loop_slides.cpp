#include "../../../include/optitrust.h"
#include <stdio.h>

int min(int a, int b) { return a < b ? a : b; }

int main() {
  for (int i = 0; i < 15; i += 3) {
    for (int j = 0; j < 15; j += 3) {
      printf("%d, %d\n", i, j);
    }
  }

  return 0;
}
