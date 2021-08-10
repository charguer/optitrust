void star( double *a, double *b, double *c, int n, int *ioff )
{
   for (int i = 0; i < n; i++ )
      a[i] *= b[i] * c[i + *ioff];
}

