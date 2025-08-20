#include <cstdint>
#include <math.h>
#include <optitrust.h>
static int GS = 32;
void quantize(int8_t *qx, float *s,  int n, float *x) {
  int num_groups = n / GS;
  float Q_MAX = 127.0f;

  for (int group = 0; group < num_groups; group++) {

    // find the max absolute value in the current group
    float wmax = 0.0;
    for (int i = 0; i < GS; i++) {
      float val = fabs(x[group * GS + i]);
      if (val > wmax) {
        wmax = val;
      }
    }

    // calculate and write the scaling factor
    float scale = wmax / Q_MAX;
    s[group] = scale;

    // calculate and write the quantized values
    for (int i = 0; i < GS; i++) {
      float quant_value = x[group * GS + i] / scale; // scale
      int8_t quantized = (int8_t)round(quant_value); // round and clamp
      qx[group * GS + i] = quantized;
    }
  }
}

void matvec_quantized(float *xout, int8_t *qx,float *s_x, int8_t *w, float *s_w, int n, int d) {
  // W (d,n) @ x (n,) -> xout (d,)
  // by far the most amount of time is spent inside this little function
  // inputs to this function are both quantized

  int i;
#pragma omp parallel for private(i)
  for (i = 0; i < d; i++) {

    float val = 0.0f;
    int32_t ival = 0;
    int in = i * n;

    // do the matmul in groups of GS
    int j;
    for (j = 0; j <= n - GS; j += GS) {
      for (int k = 0; k < GS; k++) {
        ival += ((int32_t)qx[j + k]) * ((int32_t)w[in + j + k]);
      }
      val += ((float)ival) * s_w[(in + j) / GS] * s_x[j / GS];
      ival = 0;
    }

    xout[i] = val;
  }
}

void matvec(float *xout, float *x, float *w, int n, int d) {
  for (int i = 0; i < d; i++) {
    xout[i] = 0;
    int in = i * n;
    for (int j = 0; j < n; j++) {
      xout[i] += x[j] * w[in + j];
    }
  }
}
void matvec_quantized_wrapper(float *xout, float *x, int8_t *w, float *s_w,
                              int n, int d) {
  int8_t *qx = (int8_t *)calloc(n, sizeof(int8_t));
  float *s_x = (float *)calloc(n, sizeof(float));
  quantize(qx, s_x, n, x);
  matvec_quantized(xout, qx, s_x,w, s_w, n, d);
  free(qx);
  free(s_x);
}

void test_f_in(int * const x_int, float *x) {
  for (int i = 0; i < 5; i++) {
    x_int[i] = (int)x[i];
  }
}

void test_f(float *x_out, int *x_int) {
  for (int i = 0; i < 5; i++) {
    x_out[i] = (float)(x_int[i] * 12);
  }
}

void test_f_before(float *x_out, float *x) {
  for (int i = 0; i < 5; i++) {
    x_out[i] = (x[i] * 12);
  }
}

int main() {
  int const n = 10;
  int const d = 100;
  float *w = (float *)calloc(n * d, sizeof(float));
  int8_t *const qw = (int8_t *)calloc(n * d, sizeof(int8_t));
  float *const s_w = (float *)calloc(n * d / GS, sizeof(float));
  float * const x = MALLOC1(float,n);
  float * const xout = MALLOC1(float,n);
  matvec(xout, x, w, n, d);
}
