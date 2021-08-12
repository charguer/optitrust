double foo(int i, int j);

void work( int N, int M, double **A, double **B, double **C )
{
  double alpha = 1.2;

  for (int i = 1; i < N-1; i++)
  {
    for (int j = 1; j < M-1; j++)
    {
      A[i][j] = foo(i, j);

      B[i][j] = alpha * A[i][j];

      C[i][j] = 0.2 * (A[i-1][j] + A[i+1][j] +
                A[i][j-1] + A[i][j+1] + A[i][j]);
    }
  }
}