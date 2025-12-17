#include <math.h>
#include <optitrust.h>

float _powf(float x, float pow) {
  __pure();
  __admitted();
  return powf(x, pow);
}

float _cosf(float x) {
  __pure();
  __admitted();
  return cos(x);
}

float _sinf(float x) {
  __pure();
  __admitted();
  return sin(x);
}

float _sqrtf(float x) {
  __pure();
  __admitted();
  return sqrtf(x);
}
float _expf(float x) {
  __pure();
  __admitted();
  return expf(x);
}
void softmax(int col_count, int col_stride, float *x) {
  __modifies("x ~>Matrix1(col_count)");
  float max_val = x[MINDEX1(col_count, 0)];
    __GHOST_BEGIN(focus_subrange, group_focus_subrange,
                "sub_range := 1..col_count");
  for (int j = 1; j < col_count; j++) {
    __xmodifies("&x[MINDEX1(col_count,j)] ~> Cell");
    if (x[MINDEX1(col_count, j)] > max_val) {
      max_val = x[MINDEX1(col_count, j)];
    }
  }
  __GHOST_END(focus_subrange);
  // exp and sum
  float sum = 0.0f;
  for (int j = 0; j < col_count; j++) {
    x[MINDEX1(col_count, j)] = _expf(x[MINDEX1(col_count, j)] - max_val);
    sum += x[MINDEX1(col_count, j)];
  }
  // normalize
  for (int j = 0; j < col_count; j++) {
    x[MINDEX1(col_count, j)] /= sum;
  }
}
