#include <optitrust.h>

void add(int* out, int* xs, int* ys, int h, int w) {
  for (int y = 0; y < h; y++) {
    for (int x = 0; x < w; x++) {
      out[MINDEX2(h, w, y, x)] =
          xs[MINDEX2(h, w, y, x)] + ys[MINDEX2(h, w, y, x)];
    }
  }
}

void vbox(int* out, int* in_, int h, int w) {
  for (int y = 0; y < h; y++) {
    for (int x = 0; x < w - 2; x++) {
      out[MINDEX2(h, w - 2, y, x)] = in_[MINDEX2(h, w, y, x)] +
                                     in_[MINDEX2(h, w, y, x + 1)] +
                                     in_[MINDEX2(h, w, y, x + 2)];
    }
  }
}

void hbox(int* out, int* in_, int h, int w) {
  for (int y = 0; y < h - 2; y++) {
    for (int x = 0; x < w; x++) {
      out[MINDEX2(h - 2, w, y, x)] = in_[MINDEX2(h, w, y, x)] +
                                     in_[MINDEX2(h, w, y + 1, x)] +
                                     in_[MINDEX2(h, w, y + 2, x)];
    }
  }
}

void add2(int* out, int* a, int* b, int h, int w) {
  for (int y = 0; y < h; y++) {
    for (int x = 0; x < w; x++) {
      out[MINDEX2(h, w, y, x)] = a[MINDEX2(h, w, y, x)] +
                                 b[MINDEX2(h, w, y, x)] +
                                 b[MINDEX2(h, w, y, x)];
    }
  }
}

void add2vbox(int* out, int* a, int* b, int h, int w) {
  for (int y = 0; y < h; y++) {
    for (int x = 0; x < w - 2; x++) {
      out[MINDEX2(h, w - 2, y, x)] =
          a[MINDEX2(h, w, y, x)] + a[MINDEX2(h, w, y, x + 1)] +
          a[MINDEX2(h, w, y, x + 2)] +
          (b[MINDEX2(h, w, y, x)] + b[MINDEX2(h, w, y, x + 1)] +
           b[MINDEX2(h, w, y, x + 2)]);
    }
  }
}

void vboxadd(int* out, int* a, int* b, int h, int w) {
  for (int y = 0; y < h; y++) {
    for (int x = 0; x < w - 2; x++) {
      int* const ab = (int*)MALLOC2(1, 3, sizeof(int));
      for (int x_ab = 0; x_ab < 3; x_ab++) {
        ab[MINDEX2(1, 3, 0, x_ab)] =
            a[MINDEX2(h, w, y, x_ab + x)] + b[MINDEX2(h, w, y, x_ab + x)];
      }
      out[MINDEX2(h, w - 2, y, x)] = ab[MINDEX2(1, 3, 0, 0)] +
                                     ab[MINDEX2(1, 3, 0, 1)] +
                                     ab[MINDEX2(1, 3, 0, 2)];
      MFREE2(1, 3, ab);
    }
  }
}

void hboxadd(int* out, int* a, int* b, int h, int w) {
  for (int y = 0; y < h - 2; y += 32) {
    int* const ab = (int*)MALLOC2(34, w, sizeof(int));
    for (int y_out = 0; y_out < min(h, y + 34) - y; y_out++) {
      for (int x = 0; x < w; x++) {
        ab[MINDEX2(34, w, y_out, x)] =
            a[MINDEX2(h, w, y_out + y, x)] + b[MINDEX2(h, w, y_out + y, x)];
      }
      if (min(h, y + 34) - min(h - 2, y + 32) <= y_out) {
        for (int x = 0; x < w; x++) {
          out[MINDEX2(h - 2, w, y_out - min(h, y + 34) + y + min(h - 2, y + 32),
                      x)] =
              ab[MINDEX2(34, w, y_out - min(h, y + 34) + min(h - 2, y + 32),
                         x)] +
              ab[MINDEX2(34, w, y_out - min(h, y + 34) + min(h - 2, y + 32) + 1,
                         x)] +
              ab[MINDEX2(34, w, y_out - min(h, y + 34) + min(h - 2, y + 32) + 2,
                         x)];
        }
      }
    }
    MFREE2(34, w, ab);
  }
}
