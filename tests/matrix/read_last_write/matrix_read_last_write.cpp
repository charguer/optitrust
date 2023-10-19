#include "../../include/optitrust.h"

int main() {
  int* t = (int*) MALLOC1(2, sizeof(int));
  for (int i = 0; i < 2; i++) {
    t[MINDEX1(2, i)] = i;
  }
  for (int i = 0; i < 2; i++) {
    int l = t[MINDEX1(2, i)];
  }

  int* t2 = (int*) MALLOC1(3, sizeof(int));
  for (int i = 0; i < 3; i++) {
    t2[MINDEX1(3, i)] = i;
  }
  int r = t2[MINDEX1(3, 0)] + t2[MINDEX1(3, 1)] + t2[MINDEX1(3, 2)];

  int* t3 = (int*) MALLOC2(1, 3, sizeof(int));
  for (int i = 0; i < 3; i++) {
    t3[MINDEX2(1, 3, 0, i)] = i;
  }
  int r2 = t3[MINDEX2(1, 3, 0, 0)] + t3[MINDEX2(1, 3, 0, 1)] + t3[MINDEX2(1, 3, 0, 2)];

  float* img = (float*) MALLOC2(8, 8, sizeof(float));
  for (int y = 0; y < 8; y++) {
    for (int x = 0; x < 8; x++) {
      img[MINDEX2(8, 8, y, x)] = y + x;
    }
  }
  for (int y = 0; y < 6; y++) {
    for (int x = 0; x < 6; x++) {
      float acc = 0.0f;
      for (int yd = 0; yd < 3; yd++) {
        for (int xd = 0; xd < 3; xd++) {
          acc += img[MINDEX2(8, 8, y + yd, x + xd)];
        }
      }
    }
  }

  free(img);
  free(t);
  free(t2);
  free(t3);
  return 0;
}