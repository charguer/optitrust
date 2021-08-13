float dotprod(float B[], float C[], int N)
{
   float sum = 0;
   for (int i=0; i<N; i++)
      sum += B[i] * C[i];
   return sum;
}
