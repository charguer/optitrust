

typedef int T;
typedef uchar ST;

/*
  cn: number of channel
  ksize:
  width:
*/
void rowSum(int ksize, const T* S, ST* D, int width, int cn) {
  /* Body of the function is code taken unchanged from
     https://github.com/opencv/opencv/blob/4.x/modules/imgproc/src/box_filter.simd.hpp */
  int i = 0, k, ksz_cn = ksize*cn;
  width = (width - 1)*cn;
  for( k = 0; k < cn; k++, S++, D++)
  {
      ST s = 0;
      for( i = 0; i < ksz_cn; i += cn )
          s += (ST)S[i];
      D[0] = s;
      for( i = 0; i < width; i += cn )
      {
          s += (ST)S[i + ksz_cn] - (ST)S[i];
          D[i+cn] = s;
      }
  }
}

/* Step 1: rewritten by hand */

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

/* Step 1: rewritten by hand using 2D? */

void rowSum_NORMALIZED(const int ksize, const T* S0, ST* D0, const int width, const int cn) {
  const int ksz_cn = ksize * cn;
  const int i_bound = (width - 1)*cn;

  D[i][k] =



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




  for(int k = 0; k < cn; k++)
    for(int i = 0; i < i_bound; i += cn) {

for(int i = 0; i < (width-1)*cn; i += cn) {
  for(int k = 0; k < cn; k++)
    M[i + ksize*cn]


for(int i = 0; i < ksz_cn; i += cn) {
  for(int k = 0; k < cn; k++)
    M[i + ksize*cn]


/* Specialization ksize==3 */


for( i = 0; i < width + cn; i++ )
{
  D[i] = (ST)S[i] + (ST)S[i+cn] + (ST)S[i+cn*2];
}




/* Step 1: go to optitrust compatible code */

void rowSum_NORMALIZED(const int ksize, const T* S, ST* D, const int width, const int cn) {
  const int ksz_cn = ksize * cn;
  const int i_bound = (width - 1)*cn;
  for(int k = 0; k < cn; k++)
  {
    ST s = 0;
    for(int i = 0; i < ksz_cn; i += cn) {
      s += (ST) S[MINDEX1(? ,i)];
    }
    D[0] = s;
    for(int i = 0; i < i_bound; i += cn) {
      s += (ST) S[MINDEX1(?, i + ksz_cn)] - (ST) S[MINDEX(?, i)];
      D[i + cn] = s;
    }
    S++;
    D++;
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

 ST s[3] = 0;

    T* const S = S0 + k
    ST* const D = DO + k;
  for(int k = 0; k < cn; k++)
  {
    for(int i = 0; i < ksz_cn; i += cn) {
      s += (ST) S[MINDEX1(? ,i)];
    }
    D[0] = s;
    for(int i = 0; i < i_bound; i += cn) {
      s += (ST) S[MINDEX1(?, i + ksz_cn)] - (ST) S[MINDEX(?, i)];
      D[i + cn] = s;
    }
  }
}

D[0]
D++
D[0]
D++
D[0]

D[0]
D[1]
D[2]
D += 3




x = x0
for (i=start i++)
  x++
  y = x

for i
  y = x0 + i - start




  i += B

  i = bi*B







            ST s0 = 0, s1 = 0, s2 = 0;
            for( i = 0; i < ksz_cn; i += 3 )
            {
                s0 += (ST)S[i];
                s1 += (ST)S[i+1];
                s2 += (ST)S[i+2];
            }
            D[0] = s0;
            D[1] = s1;
            D[2] = s2;
            for( i = 0; i < width; i += 3 )
            {
                s0 += (ST)S[i + ksz_cn] - (ST)S[i];
                s1 += (ST)S[i + ksz_cn + 1] - (ST)S[i + 1];
                s2 += (ST)S[i + ksz_cn + 2] - (ST)S[i + 2];
                D[i+3] = s0;
                D[i+4] = s1;
                D[i+5] = s2;
            }
        }

















  /* Manually optimized code

        const T* S = (const T*)src;
        ST* D = (ST*)dst;
        int i = 0, k, ksz_cn = ksize*cn;

        width = (width - 1)*cn;
        if( ksize == 3 )
        {
            for( i = 0; i < width + cn; i++ )
            {
                D[i] = (ST)S[i] + (ST)S[i+cn] + (ST)S[i+cn*2];
            }
        }
        else if( ksize == 5 )
        {
            for( i = 0; i < width + cn; i++ )
            {
                D[i] = (ST)S[i] + (ST)S[i+cn] + (ST)S[i+cn*2] + (ST)S[i + cn*3] + (ST)S[i + cn*4];
            }
        }
        else if( cn == 1 )
        {
            ST s = 0;
            for( i = 0; i < ksz_cn; i++ )
                s += (ST)S[i];
            D[0] = s;
            for( i = 0; i < width; i++ )
            {
                s += (ST)S[i + ksz_cn] - (ST)S[i];
                D[i+1] = s;
            }
        }
        else if( cn == 3 )
        {
            ST s0 = 0, s1 = 0, s2 = 0;
            for( i = 0; i < ksz_cn; i += 3 )
            {
                s0 += (ST)S[i];
                s1 += (ST)S[i+1];
                s2 += (ST)S[i+2];
            }
            D[0] = s0;
            D[1] = s1;
            D[2] = s2;
            for( i = 0; i < width; i += 3 )
            {
                s0 += (ST)S[i + ksz_cn] - (ST)S[i];
                s1 += (ST)S[i + ksz_cn + 1] - (ST)S[i + 1];
                s2 += (ST)S[i + ksz_cn + 2] - (ST)S[i + 2];
                D[i+3] = s0;
                D[i+4] = s1;
                D[i+5] = s2;
            }
        }
        else if( cn == 4 )
        {
            ST s0 = 0, s1 = 0, s2 = 0, s3 = 0;
            for( i = 0; i < ksz_cn; i += 4 )
            {
                s0 += (ST)S[i];
                s1 += (ST)S[i+1];
                s2 += (ST)S[i+2];
                s3 += (ST)S[i+3];
            }
            D[0] = s0;
            D[1] = s1;
            D[2] = s2;
            D[3] = s3;
            for( i = 0; i < width; i += 4 )
            {
                s0 += (ST)S[i + ksz_cn] - (ST)S[i];
                s1 += (ST)S[i + ksz_cn + 1] - (ST)S[i + 1];
                s2 += (ST)S[i + ksz_cn + 2] - (ST)S[i + 2];
                s3 += (ST)S[i + ksz_cn + 3] - (ST)S[i + 3];
                D[i+4] = s0;
                D[i+5] = s1;
                D[i+6] = s2;
                D[i+7] = s3;
            }
        }
        else
            for( k = 0; k < cn; k++, S++, D++ )
            {
                ST s = 0;
                for( i = 0; i < ksz_cn; i += cn )
                    s += (ST)S[i];
                D[0] = s;
                for( i = 0; i < width; i += cn )
                {
                    s += (ST)S[i + ksz_cn] - (ST)S[i];
                    D[i+cn] = s;
                }
            }
    }
  */
