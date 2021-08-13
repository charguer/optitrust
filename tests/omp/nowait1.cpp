#include <math.h>
void nowait_example(int n, int m, float *a, float *b, float *y, float *z)
{
      for (int i=1; i<n; i++)
        b[i] = (a[i] + a[i-1]) / 2.0;

      for (int j=0; j<m; j++)
        y[j] = sqrt(z[j]);
}

