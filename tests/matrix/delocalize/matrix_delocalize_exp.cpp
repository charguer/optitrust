#include "../../../include/optitrust.h"

typedef int T;

T* b;

const int N0 = 5;

const int N1 = 10;

const int N2 = 10;

const int N3 = 10;

T* y;

void allocate() {
  b = (T*)CALLOC3(N1, N2, N3, sizeof(T));
  y = (T*)CALLOC4(N0, N1, N2, N3, sizeof(T));
}

int main() {
  T* a = (T*)CALLOC3(N1, N2, N3, sizeof(T));
  T* x = (T*)CALLOC4(N0, N1, N2, N3, sizeof(T));
  for (int i1 = 0; i1 < N1; i1++) {
    for (int i2 = 0; i2 < N2; i2++) {
      for (int i3 = 0; i3 < N3; i3++) {
        for (int i0 = 0; i0 < N0; i0++) {
          x[MINDEX4(N0, N1, N2, N3, i0, i1, i2, i3)] = 0;
        }
      }
    }
  }
  for (int i = 0; i < 10; i++) {
    int t = x[MINDEX4(N0, N1, N2, N3, ANY(N0), i, i + 1, i + 2)];
    x[MINDEX4(N0, N1, N2, N3, ANY(N0), i, i + 1, i + 2)] = t + 5;
  }
  for (int i1 = 0; i1 < N1; i1++) {
    for (int i2 = 0; i2 < N2; i2++) {
      for (int i3 = 0; i3 < N3; i3++) {
        double sum = 0;
        for (int i0 = 0; i0 < N0; i0++) {
          sum += x[MINDEX4(N0, N1, N2, N3, i0, i1, i2, i3)];
        }
        a[MINDEX3(N1, N2, N3, i1, i2, i3)] = sum;
      }
    }
  }
  free(x);
  for (int i1 = 0; i1 < N1; i1++) {
    for (int i2 = 0; i2 < N2; i2++) {
      for (int i3 = 0; i3 < N3; i3++) {
        for (int i0 = 0; i0 < N0; i0++) {
          y[MINDEX4(N0, N1, N2, N3, i0, i1, i2, i3)] = 0;
        }
      }
    }
  }
  for (int j = 0; j < 10; j++) {
    y[MINDEX4(N0, N1, N2, N3, ANY(N0), j, j + 1, j + 2)];
  }
  for (int i1 = 0; i1 < N1; i1++) {
    for (int i2 = 0; i2 < N2; i2++) {
      for (int i3 = 0; i3 < N3; i3++) {
        double sum = 0;
        for (int i0 = 0; i0 < N0; i0++) {
          sum += y[MINDEX4(N0, N1, N2, N3, i0, i1, i2, i3)];
        }
        b[MINDEX3(N1, N2, N3, i1, i2, i3)] = sum;
      }
    }
  }
  free(y);
  int z = 0;
  return 0;
}
