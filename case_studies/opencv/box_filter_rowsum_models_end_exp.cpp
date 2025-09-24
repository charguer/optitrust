#include <optitrust_models.h>

void rowSum(int w, int* s, int* d, int n, int cn) {
  if (w == 3) {
    for (int ic = 0; ic < n * cn; ic++) {
      int sum = 0;
      sum += s[ic] + s[cn + ic] + s[2 * cn + ic];
      d[ic] = sum;
    }
  } else {
    if (w == 5) {
      for (int ic = 0; ic < n * cn; ic++) {
        int sum = 0;
        sum += s[ic] + s[cn + ic] + s[2 * cn + ic] + s[3 * cn + ic] +
               s[4 * cn + ic];
        d[ic] = sum;
      }
    } else {
      if (cn == 1) {
        int sum = 0;
        for (int k = 0; k < w; k++) {
          sum += s[k];
        }
        d[0] = sum;
        for (int i = 0; i < n - 1; i += 1) {
          sum += s[w + i] - s[i];
          d[i + 1] = sum;
        }
      } else {
        if (cn == 3) {
          int sum = 0;
          int sum7 = 0;
          int sum10 = 0;
          for (int k = 0; k < 3 * w; k += 3) {
            sum += s[k];
            sum7 += s[k + 1];
            sum10 += s[k + 2];
          }
          d[0] = sum;
          d[1] = sum7;
          d[2] = sum10;
          for (int i = 0; i < 3 * n - 3; i += 3) {
            sum += s[3 * w + i] - s[i];
            sum7 += s[3 * w + i + 1] - s[i + 1];
            sum10 += s[3 * w + i + 2] - s[i + 2];
            d[i + 3] = sum;
            d[i + 4] = sum7;
            d[i + 5] = sum10;
          }
        } else {
          if (cn == 4) {
            int sum = 0;
            int sum13 = 0;
            int sum16 = 0;
            int sum19 = 0;
            for (int k = 0; k < 4 * w; k += 4) {
              sum += s[k];
              sum13 += s[k + 1];
              sum16 += s[k + 2];
              sum19 += s[k + 3];
            }
            d[0] = sum;
            d[1] = sum13;
            d[2] = sum16;
            d[3] = sum19;
            for (int i = 0; i < 4 * n - 4; i += 4) {
              sum += s[4 * w + i] - s[i];
              sum13 += s[4 * w + i + 1] - s[i + 1];
              sum16 += s[4 * w + i + 2] - s[i + 2];
              sum19 += s[4 * w + i + 3] - s[i + 3];
              d[i + 4] = sum;
              d[i + 5] = sum13;
              d[i + 6] = sum16;
              d[i + 7] = sum19;
            }
          } else {
            for (int c = 0; c < cn; c++) {
              int sum = 0;
              for (int k = 0; k < w * cn; k += cn) {
                sum += s[c + k];
              }
              d[c] = sum;
              for (int i = c; i < cn * n - cn + c; i += cn) {
                sum += s[w * cn + i] - s[i];
                d[cn + i] = sum;
              }
            }
          }
        }
      }
    }
  }
}
