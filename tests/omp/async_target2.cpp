#include <stdlib.h>
#include <omp.h>
extern void init(float *, float *, int);
extern void foo();
extern void output(float *, int);
void vec_mult(float *p, int N, int dev)
{
   float *v1, *v2;
   int i;
   {
       // check whether on device dev
       if (omp_is_initial_device())
          abort();
       v1 = (float *)malloc(N*sizeof(float));
       v2 = (float *)malloc(N*sizeof(float));
       init(v1, v2, N);
   }
   foo(); // execute other work asychronously
   {
       // check whether on device dev
       if (omp_is_initial_device())
          abort();
       for (int i=0; i<N; i++)
         p[i] = v1[i] * v2[i];
       free(v1);
       free(v2);
   }
   output(p, N);
}