#include <optitrust.h>
#include <stdio.h>

int main() {
  for (int bi = 0; bi < 15 + 3 * (1 - 2); bi += 1 * 3) {
    for (int bj = 0; bj < 15 + 3 * (2 - 3); bj += 2 * 3) {
      for (int i = 0; i < 2 * 3; i += 3) {
        for (int j = 0; j < min(15, bj + 3 * 3) - bj; j += 3) {
          printf("%d, %d\n", i + bi, j + bj);
        }
      }
    }
  }
  return 0;
}
