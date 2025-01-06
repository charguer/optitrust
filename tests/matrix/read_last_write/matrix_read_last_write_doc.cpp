#include <optitrust.h>

int main() {
  int* t = MALLOC1(int, 2);
  for (int i = 0; i < 2; i++) {
    t[MINDEX1(2, i)] = i;
  }
  for (int i = 0; i < 2; i++) {
    int l = t[MINDEX1(2, i)];
  }
}
