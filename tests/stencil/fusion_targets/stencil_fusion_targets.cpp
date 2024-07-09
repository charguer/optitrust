#include <optitrust.h>

void add(int* out, int* xs, int* ys, int h, int w) {
  __writes("out ~> Matrix2(h, w)");
  __reads("xs ~> Matrix2(h, w)");
  __reads("ys ~> Matrix2(h, w)");

  for (int y = 0; y < h; y++) {
    __strict();
    __xwrites("for x in 0..w -> &out[MINDEX2(h, w, y, x)] ~> Cell");
    __sreads("xs ~> Matrix2(h, w)");
    __sreads("ys ~> Matrix2(h, w)");
    for (int x = 0; x < w; x++) {
      __strict();
      __xwrites("&out[MINDEX2(h, w, y, x)] ~> Cell");
      __sreads("xs ~> Matrix2(h, w)");
      __sreads("ys ~> Matrix2(h, w)");
      __GHOST_BEGIN(xsf, matrix2_ro_focus, "xs, y, x");
      __GHOST_BEGIN(ysf, matrix2_ro_focus, "ys, y, x");
      out[MINDEX2(h, w, y, x)] = xs[MINDEX2(h, w, y, x)] + ys[MINDEX2(h, w, y, x)];
      __GHOST_END(xsf);
      __GHOST_END(ysf);
    }
  }
}

void vbox(int* out, int* in_, int h, int w) {
  __writes("out ~> Matrix2(h, w-2)");
  __reads("in_ ~> Matrix2(h, w)");

  for (int y = 0; y < h; y++) {
    __strict();
    __xwrites("for x in 0..(w-2) -> &out[MINDEX2(h, w-2, y, x)] ~> Cell");
    __sreads("in_ ~> Matrix2(h, w)");
    for (int x = 0; x < w - 2; x++) {
      __strict();
      __xwrites("&out[MINDEX2(h, w-2, y, x)] ~> Cell");
      __sreads("in_ ~> Matrix2(h, w)");

      __ghost(in_range_extend, "x, 0..(w-2), 0..w");
      __ghost(in_range_shift_extend, "x, 1, 0..w, 0, w-2");
      __ghost(in_range_shift_extend, "x, 2, 0..w, 0, w-2");
      __GHOST_BEGIN(in0, matrix2_ro_focus, "in_, y, x");
      __GHOST_BEGIN(in1, matrix2_ro_focus, "in_, y, x+1");
      __GHOST_BEGIN(in2, matrix2_ro_focus, "in_, y, x+2");
      out[MINDEX2(h, w-2, y, x)] = in_[MINDEX2(h, w, y, x)] + in_[MINDEX2(h, w, y, x+1)] + in_[MINDEX2(h, w, y, x+2)];
      __GHOST_END(in0);
      __GHOST_END(in1);
      __GHOST_END(in2);
    }
  }
}

void hbox(int* out, int* in_, int h, int w) {
  __writes("out ~> Matrix2(h-2, w)");
  __reads("in_ ~> Matrix2(h, w)");

  for (int y = 0; y < h - 2; y++) {
    __strict();
    __xwrites("for x in 0..w -> &out[MINDEX2(h-2, w, y, x)] ~> Cell");
    __sreads("in_ ~> Matrix2(h, w)");
    for (int x = 0; x < w; x++) {
      __strict();
      __xwrites("&out[MINDEX2(h-2, w, y, x)] ~> Cell");
      __sreads("in_ ~> Matrix2(h, w)");

      __ghost(in_range_extend, "y, 0..(h-2), 0..h");
      __ghost(in_range_shift_extend, "y, 1, 0..h, 0, h-2");
      __ghost(in_range_shift_extend, "y, 2, 0..h, 0, h-2");
      __GHOST_BEGIN(in0, matrix2_ro_focus, "in_, y, x");
      __GHOST_BEGIN(in1, matrix2_ro_focus, "in_, y+1, x");
      __GHOST_BEGIN(in2, matrix2_ro_focus, "in_, y+2, x");
      out[MINDEX2(h-2, w, y, x)] = in_[MINDEX2(h, w, y, x)] + in_[MINDEX2(h, w, y+1, x)] + in_[MINDEX2(h, w, y+2, x)];
      __GHOST_END(in0);
      __GHOST_END(in1);
      __GHOST_END(in2);
    }
  }
}

void add2(int* out, int* a, int* b, int h, int w) {
  __writes("out ~> Matrix2(h, w)");
  __reads("a ~> Matrix2(h, w)");
  __reads("b ~> Matrix2(h, w)");

  int* const ab = (int*) MALLOC2(h, w, sizeof(int));
  add(ab, a, b, h, w);
  add(out, ab, b, h, w);
  MFREE2(h, w, ab);
}

void add2vbox(int* out, int* a, int* b, int h, int w) {
  __writes("out ~> Matrix2(h, w-2)");
  __reads("a ~> Matrix2(h, w)");
  __reads("b ~> Matrix2(h, w)");

  int* const box_a = (int*) MALLOC2(h, w-2, sizeof(int));
  int* const box_b = (int*) MALLOC2(h, w-2, sizeof(int));
  vbox(box_a, a, h, w);
  vbox(box_b, b, h, w);
  add(out, box_a, box_b, h, w-2);
  MFREE2(h, w-2, box_a);
  MFREE2(h, w-2, box_b);
}

void vboxadd(int* out, int* a, int* b, int h, int w) {
  __writes("out ~> Matrix2(h, w-2)");
  __reads("a ~> Matrix2(h, w)");
  __reads("b ~> Matrix2(h, w)");

  int* const ab = (int*) MALLOC2(h, w, sizeof(int));
  add(ab, a, b, h, w);
  vbox(out, ab, h, w);
  MFREE2(h, w, ab);
}

void hboxadd(int* out, int* a, int* b, int h, int w) {
  __writes("out ~> Matrix2(h-2, w)");
  __reads("a ~> Matrix2(h, w)");
  __reads("b ~> Matrix2(h, w)");

  int* const ab = (int*) MALLOC2(h, w, sizeof(int));
  add(ab, a, b, h, w);
  hbox(out, ab, h, w);
  MFREE2(h, w, ab);
}
