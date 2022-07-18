#define THRESHOLD 1000000
extern void init(float*, float*, int);
extern void output(float*, int);
void vec_mult(float *p, float *v1, float *v2, int N)
{
   int i;
   init(v1, v2, N);
   for (i=0; i<N; i++)
     p[i] = v1[i] * v2[i];
   /* UNDEFINED behavior if N<=THRESHOLD */
   output(p, N);
}
