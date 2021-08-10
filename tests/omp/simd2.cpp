#include <stdio.h>

double add1(double a, double b, double fact)
{
   double c;
   c = a + b + fact;
   return c;
}

double add2(double *a, double *b, int i, double fact)
{
   double c;
   c = a[i] + b[i] + fact;
   return c;
}

double add3(double *a, double *b, double fact)
{
   double c;
   c = *a + *b + fact;
   return c;
}

void work( double *a, double *b, int n )
{
   double tmp;
   #pragma omp simd private(tmp)
   for (int i = 0; i < n; i++ ) {
      tmp  = add1( a[i],  b[i], 1.0);
      a[i] = add2( a,     b, i, 1.0) + tmp;
      a[i] = add3(&a[i], &b[i], 1.0);
   }
}

int main(){
   int i;
   const int N=32;
   double a[N], b[N];
 
   for ( i=0; i<N; i++ ) {
      a[i] = i; b[i] = N-i;
   }
 
   work(a, b, N );
 
   for ( i=0; i<N; i++ ) {
      printf("%d %f\n", i, a[i]);
   }

   return 0;
}
