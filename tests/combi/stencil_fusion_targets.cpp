#include "../../include/optitrust.h"

void add(int* out, int* xs, int* ys, int h, int w) {
  for (int x = 0; x < h; x++) {
    for (int y = 0; y < w; y++) {
      out[MINDEX2(h, w, y, x)] = xs[MINDEX2(h, w, y, x)] + ys[MINDEX2(h, w, y, x)];
    }
  }
}

void add2(int* out, int* a, int* b, int h, int w) {
  int* tmp = (int*) MALLOC2(h, w, sizeof(int));
  add(tmp, a, b, h, w);
  add(out, tmp, b, h, w);
  free(tmp);
}