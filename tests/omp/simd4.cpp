void work( float *b, int n, int m )
{
   for (int i = m; i < n; i++)
      b[i] = b[i-m] - 1.0f;
}