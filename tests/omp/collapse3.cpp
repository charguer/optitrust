#include <omp.h>
#include <stdio.h>
void work(int a, int j, int k);
void sub()
{
  int  a;
  for (int k=1; k<=3; k++)
     for (int j=1; j<=2; j++)
     {
        printf("%d %d %d\n", omp_get_thread_num(), k, j);
        /* end ordered */
        work(a,j,k);
     }
}
