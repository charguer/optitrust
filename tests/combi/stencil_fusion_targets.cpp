#include "../../include/optitrust.h"

void add(int* out, int* xs, int* ys, int h, int w) {
  for (int y = 0; y < h; y++) {
    for (int x = 0; x < w; x++) {
      out[MINDEX2(h, w, y, x)] = xs[MINDEX2(h, w, y, x)] + ys[MINDEX2(h, w, y, x)];
    }
  }
}

void vbox(int* out, int* in, int h, int w) {
  for (int y = 0; y < h; y++) {
    for (int x = 0; x < w - 2; x++) {
      out[MINDEX2(h, w-2, y, x)] = in[MINDEX2(h, w, y, x)] + in[MINDEX2(h, w, y, x+1)] + in[MINDEX2(h, w, y, x+2)];
    }
  }
}

void hbox(int* out, int* in, int h, int w) {
  for (int y = 0; y < h - 2; y++) {
    for (int x = 0; x < w; x++) {
      out[MINDEX2(h, w, y-2, x)] = in[MINDEX2(h, w, y, x)] + in[MINDEX2(h, w, y+1, x)] + in[MINDEX2(h, w, y+2, x)];
    }
  }
}

void add2(int* out, int* a, int* b, int h, int w) {
  int* ab = (int*) MALLOC2(h, w, sizeof(int));
  add(ab, a, b, h, w);
  add(out, ab, b, h, w);
  free(ab);
}

void add2vbox(int* out, int* a, int* b, int h, int w) {
  int* box_a = (int*) MALLOC2(h, w-2, sizeof(int));
  int* box_b = (int*) MALLOC2(h, w-2, sizeof(int));
  vbox(box_a, a, h, w);
  vbox(box_b, b, h, w);
  add(out, box_a, box_b, h, w-2);
  free(box_a);
  free(box_b);
}

void vboxadd(int* out, int* a, int* b, int h, int w) {
  int* ab = (int*) MALLOC2(h, w, sizeof(int));
  add(ab, a, b, h, w);
  vbox(out, ab, h, w);
  free(ab);
}

void hboxadd(int* out, int* a, int* b, int h, int w) {
  int* ab = (int*) MALLOC2(h, w, sizeof(int));
  add(ab, a, b, h, w);
  hbox(out, ab, h, w);
  free(ab);
}