#include <optitrust.h>

typedef uint8_t T;
typedef uint16_t ST;

/*
  cn: number of (color) channels
  ksize: size of box filter (convolution window)
  width: size of the row resulting from filtering
*/
void rowSum(const int ksize, const T* S, ST* D, const int width, const int cn) {
  for (int k = 0; k < cn; k++) { // foreach channel
    // initialize the sliding window
    ST s = 0;
    for (int i = 0; i < ksize; i++) {
      s += (ST) S[MINDEX2(width+ksize, cn, i, k)];
    }
    D[MINDEX2(width, cn, 0, k)] = s;
    // for each pixel, shift the sliding window
    for (int i = 0; i < width-1; i++) {
      s -= (ST) S[MINDEX2(width+ksize, cn, i, k)];
      s += (ST) S[MINDEX2(width+ksize, cn, i + ksize, k)];
      D[MINDEX2(width, cn, i + 1, k)] = s;
    }
    /* ALTERNATIVE
    for (int i = 1; i < width; i++) {
      s -= (ST) S[MINDEX2(?, cn, i - 1, k)];
      s += (ST) S[MINDEX2(?, cn, i + ksize-1, k)];
      D[MINDEX2(?, cn, i, k] = s;
    }
    */
  }
}

void rowSumOpt(const int ksize, const T* S, ST* D, const int width, const int cn) {
  // introduce arbitrary conditions
  /* if (ksize == 3) {
    // in this section, can do the substitution, or insert the line "const int ksize = 3;".
    // ...
  } else if (ksize == 5) {
    // ...
  } else */ if (cn == 1) {
    rowSum(ksize, S, D, width, 1);
  } else if (cn == 3) {
    rowSum(ksize, S, D, width, 3);
  } else {
    rowSum(ksize, S, D, width, cn);
  }
}
