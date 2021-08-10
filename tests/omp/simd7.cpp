#include <stdio.h>
#include <stdlib.h>
 
const int N = 45;
int a[N], b[N], c[N];

int fib( int n )
{
   if (n <= 1)
      return n;
   else {
      return fib(n-1) + fib(n-2);
   }
}
 
int main(void)
{

   for (int i=0; i < N; i++) b[i] = i;

   for (int i=0; i < N; i++) {
      a[i] = fib(b[i]);
   }
   printf("Done a[%d] = %d\n", N-1, a[N-1]);
   return 0;
}
