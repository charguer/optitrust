#include "harris.h"
#include <stdlib.h>
#include "../../include/optitrust.h"

void conv3x3(float* out,
             int h, int w,
             const float* in,
             const float* weights)
{
    for (int y = 0; y < (h - 2); y++) {
        // int r0 = (y + 0) * w;
        // int r1 = (y + 1) * w;
        // int r2 = (y + 2) * w;
        for (int x = 0; x < (w - 2); x++) {
            // int c0 = x + 0;
            // int c1 = x + 1;
            // int c2 = x + 2;
            out[y*(w - 2)+x] = (
                weights[MINDEX2(3, 3, 0, 0)]*in[MINDEX2(h, w, y + 0, x + 0)] + weights[MINDEX2(3, 3, 0, 1)]*in[MINDEX2(h, w, y + 0, x + 1)] + weights[MINDEX2(3, 3, 0, 2)]*in[MINDEX2(h, w, y + 0, x + 2)] +
                weights[MINDEX2(3, 3, 1, 0)]*in[MINDEX2(h, w, y + 1, x + 0)] + weights[MINDEX2(3, 3, 1, 1)]*in[MINDEX2(h, w, y + 1, x + 1)] + weights[MINDEX2(3, 3, 1, 2)]*in[MINDEX2(h, w, y + 1, x + 2)] +
                weights[MINDEX2(3, 3, 2, 0)]*in[MINDEX2(h, w, y + 2, x + 0)] + weights[MINDEX2(3, 3, 2, 1)]*in[MINDEX2(h, w, y + 2, x + 1)] + weights[MINDEX2(3, 3, 2, 2)]*in[MINDEX2(h, w, y + 2, x + 2)]
            );
        }
    }
}

void sobelX(float* out,
            int h, int w,
            const float* in)
{
    const float weights[9] = {
        -1.f/8.f, 0.f, 1.f/8.f,
        -2.f/8.f, 0.f, 2.f/8.f,
        -1.f/8.f, 0.f, 1.f/8.f
    };
    conv3x3(out, h, w, in, weights);
}

void sobelY(float* out,
            int h, int w,
            const float* in)
{
    const float weights[9] = {
        -1.f/8.f, -2.f/8.f, -1.f/8.f,
         0.f/8.f,  0.f/8.f,  0.f/8.f,
         1.f/8.f,  2.f/8.f,  1.f/8.f
    };
    conv3x3(out, h, w, in, weights);
}

void binomial(float* out,
              int h, int w,
              const float* in)
{
    const float weights[9] = {
        1.f/16.f, 2.f/16.f, 1.f/16.f,
        2.f/16.f, 4.f/16.f, 2.f/16.f,
        1.f/16.f, 2.f/16.f, 1.f/16.f
    };
    conv3x3(out, h, w, in, weights);
}

void mul(float* out,
         int h, int w,
         const float* a,
         const float* b)
{
    for (int y = 0; y < h; y++) {
        for (int x = 0; x < w; x++) {
            out[MINDEX2(h, w, y, x)] = a[MINDEX2(h, w, y, x)] * b[MINDEX2(h, w, y, x)];
        }
    }
}

void coarsity(float* out,
              int h, int w,
              const float* sxx,
              const float* sxy,
              const float* syy,
              float kappa)
{
    for (int y = 0; y < h; y++) {
        for (int x = 0; x < w; x++) {
            float det = sxx[MINDEX2(h, w, y, x)] * syy[MINDEX2(h, w, y, x)] - sxy[MINDEX2(h, w, y, x)] * sxy[MINDEX2(h, w, y, x)];
            float trace = sxx[MINDEX2(h, w, y, x)] + syy[MINDEX2(h, w, y, x)];
            out[MINDEX2(h, w, y, x)] = det - kappa * trace * trace;
        }
    }
}

void harris(float* out, int h, int w, const float* in, float kappa) {
    const int h1 = h - 2;
    const int w1 = w - 2;
    const int h2 = h - 4;
    const int w2 = w - 4;

    float* ix = (float*) MALLOC2(h1, w1, sizeof(float));
    float* iy = (float*) MALLOC2(h1, w1, sizeof(float));
    float* ixx = (float*) MALLOC2(h1, w1, sizeof(float));
    float* ixy = (float*) MALLOC2(h1, w1, sizeof(float));
    float* iyy = (float*) MALLOC2(h1, w1, sizeof(float));
    float* sxx = (float*) MALLOC2(h2, w2, sizeof(float));
    float* sxy = (float*) MALLOC2(h2, w2, sizeof(float));
    float* syy = (float*) MALLOC2(h2, w2, sizeof(float));

    sobelX(ix, h, w, in);
    sobelY(iy, h, w, in);
    mul(ixx, h1, w1, ix, ix);
    mul(ixy, h1, w1, ix, iy);
    mul(iyy, h1, w1, iy, iy);
    binomial(sxx, h1, w1, ixx);
    binomial(sxy, h1, w1, ixy);
    binomial(syy, h1, w1, iyy);
    coarsity(out, h2, w2, sxx, sxy, syy, kappa);

    free(ix);
    free(iy);
    free(ixx);
    free(ixy);
    free(iyy);
    free(sxx);
    free(sxy);
    free(syy);
}