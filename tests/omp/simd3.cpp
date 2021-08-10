double work( double *a, double *b, int n )
{
   double tmp, sum;
   sum = 0.0;
   for (int i = 0; i < n; i++) {
      tmp = a[i] + b[i];
      sum += tmp;
   }
   return sum;
}
