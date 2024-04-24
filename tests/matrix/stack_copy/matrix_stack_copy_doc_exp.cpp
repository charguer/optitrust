#include <optitrust.h>

int main() {
  float* s;
  for (int i = 0; i < 32; i++) {
    for (int j = 0; j < 32; j++) {
      for (int k = 0; k < 4; k++) {
        s[MINDEX2(32, 32, i, j)] += k;
      }
    }
  }
}
