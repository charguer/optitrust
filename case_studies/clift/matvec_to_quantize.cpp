#include <cstdint>
#include <math.h>
#include <optitrust.h>
const int GS = 32;
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
    for (int k = 0; k < n; k++) {
      xout[i] += x[k] * w[in + k];
    }
  }
}
