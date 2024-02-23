

typedef int T;
typedef uchar ST;

/*
  cn: number of channel
  ksize:
  width:
*/



void rowSum(const int ksize, const T* S, ST* D, const int width, const int cn) {
  for (int k = 0; k < cn; k++) { // foreach channel
    // initialize the sliding window
    ST s = 0;
    for (int i = 0; i < ksize; i++) {
      s += (ST) S[MINDEX2(width+ksize, cn, i, k)];
    }
    D[MINDEX2(width, cn, 0, k] = s;
    // for each pixel, shift the sliding window
    for (int i = 0; i < width-1; i++) {
      s -= (ST) S[MINDEX2(width+ksize, cn, i, k)];
      s += (ST) S[MINDEX2(?, cn, i + ksize, k)];
      D[MINDEX2(width, cn, i + 1, k] = s;
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

/* General code, with flattening */

void rowSum_NORMALIZED(const int ksize, const T* S0, ST* D0, const int width, const int cn) {
  const int ksz_cn = ksize * cn;
  const int i_bound = (width - 1)*cn;
  for(int k = 0; k < cn; k++)
  {
    T* const S = S0 + k
    ST* const D = DO + k;

    ST s = 0;
    for(int i = 0; i < ksz_cn; i += cn) {
      s += (ST) S[MINDEX1(?, i)];
    }
    D[0] = s;
    for(int i = 0; i < i_bound; i += cn) {
      s += (ST) S[MINDEX1(?, i + ksz_cn)] - (ST) S[MINDEX(?, i)];
      D[i + cn] = s;
    }
  }
}

/* Specialization ksize==3 */


void rowSum_ksize3(const int ksize, const T* S, ST* D, const int width) {
  for( i = 0; i < width + cn; i++ )
  {
    D[i] = (ST)S[i] + (ST)S[i+cn] + (ST)S[i+cn*2];
  }
}

/* Specialization ksize==5 */

void rowSum_ksize5(const int ksize, const T* S, ST* D, const int width) {
    for( i = 0; i < width + cn; i++ )
    {
        D[i] = (ST)S[i] + (ST)S[i+cn] + (ST)S[i+cn*2] + (ST)S[i + cn*3] + (ST)S[i + cn*4];
    }
}

/* Specialization cn==1 */

void rowSum_cn1(const int ksize, const T* S, ST* D, const int width) {
  const int ksz_cn = ksize;
  const int i_bound = (width - 1);
  ST s = 0;
  for (int i = 0; i < ksz_cn; i++) {
    s += (ST) S[MINDEX1(? ,i)];
  }
  D[0] = s;
  for (int i = 0; i < i_bound; i++) {
    s += (ST) S[MINDEX1(?, i + ksz_cn)] - (ST) S[MINDEX(?, i)];
    D[i + 1] = s;
  }
}

/* Specialization cn==3 */

void rowSum_cn3(const int ksize, const T* S, ST* D, const int width) {
  const int ksz_cn = ksize * cn;
  const int i_bound = (width - 1)*cn;
  /*
  .. unroll loop 3 times
  .. reorder instr
  .. fusion loops
  */
}

/* Main code */
void rowSumOpt(const int ksize, const T* S, ST* D, const int width, const int cn) {
  // introduce arbitrary conditions
  if (ksize == 3) {
    // in this section, can do the substitution, or insert the line "const int ksize = 3;".
    // ...
  } else if (ksize == 5) {
    // ...
  } else if (cn == 1) {
    // ...
  } else (cn == 3) {
    // ...
  } else {
    // ...
  }
}

// specialized_path [["ksize", int 3]; ["ksize", int 5]; ["cn", int 1]; ["cn", int 3]] tg
