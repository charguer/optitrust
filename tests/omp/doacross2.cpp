float foo(int i, int j);
float bar(float a, float b, float c);
float baz(float b);

void work( int N, int M, float **A, float **B, float **C )
{

  for (int i=1; i<N; i++)
  {
    for (int j=1; j<M; j++)
    {
      A[i][j] = foo(i, j);

      B[i][j] = bar(A[i][j], B[i-1][j], B[i][j-1]);

      C[i][j] = baz(B[i][j]);
    }
  }
}
