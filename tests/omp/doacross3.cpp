const int N = 100;

void work_wrong(double p[][N][N])
{
  int i, j, k;

  for (int i=1; i<N-1; i++)
  {
    for (int j=1; j<N-1; j++)
    {
      for (int k=1; k<N-1; k++)
      {
        double tmp1 = p[i-1][j][k] + p[i+1][j][k];
        double tmp2 = p[i][j-1][k] + p[i][j+1][k];
        double tmp3 = p[i][j][k-1] + p[i][j][k+1];
        p[i][j][k] = (tmp1 + tmp2 + tmp3) / 6.0;
      }
/* missing #pragma omp ordered depend(source) */
    }
  }
}