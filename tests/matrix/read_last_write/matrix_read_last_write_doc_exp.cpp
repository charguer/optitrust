#include <optitrust.h>

int main() {
  int* t = (int*)malloc(MSIZE1(2) * sizeof(int));
  for (int i = 0; i < 2; i++) {
    t[MINDEX1(2, i)] = i;
  }
  for (int i = 0; i < 2; i++) {
    int l = i;
  }
}
