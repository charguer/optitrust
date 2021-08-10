void work( double **a, double **b, double **c, int n )
{
   double tmp;
   for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
         tmp = a[i][j] + b[i][j];
         c[i][j] = tmp;
      }
   }
}

