#include <omp.h>
extern void init(float *, float *, int);
extern void output(float *, int);
void vec_mult(float *p, float *v1, float *v2, int N)
{
   init(v1, v2, N);
   int ndev = omp_get_num_devices();
   int do_offload = (ndev>0 && N>1000000);
   for (int i=0; i<N; i++)
     p[i] = v1[i] * v2[i];
   output(p, N);
}

