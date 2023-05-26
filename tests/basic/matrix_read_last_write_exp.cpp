#include "../../include/optitrust.h"

int main() {
  int* t = (int*)MALLOC1(2, sizeof(int));
  for (int i = 0; i < 2; i++) {
    t[MINDEX1(2, i)] = i;
  }
  for (int i = 0; i < 2; i++) {
    int l = i;
  }
  int* t2 = (int*)MALLOC1(3, sizeof(int));
  for (int i = 0; i < 3; i++) {
    t2[MINDEX1(3, i)] = i;
  }
  int r = 0 + 1 + 2;
  float* img = (float*)MALLOC2(8, 8, sizeof(float));
  for (int y = 0; y < 8; y++) {
    for (int x = 0; x < 8; x++) {
      img[MINDEX2(8, 8, y, x)] = y + x;
    }
  }
  for (int y = 0; y < 6; y++) {
    for (int x = 0; x < 6; x++) {
      float acc = 0.f;
      for (int yd = 0; yd < 3; yd++) {
        for (int xd = 0; xd < 3; xd++) {
          acc += y + yd + (x + xd);
        }
      }
    }
  }
  free(img);
  free(t);
  free(t2);
  return 0;
}
