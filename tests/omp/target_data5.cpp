void vec_mult(float* &, float* &, float* &, int &);
extern void init(float*, float*, int);
extern void output(float*, int);
void foo(float *p0, float *v1, float *v2, int N)
{
   init(v1, v2, N);
   vec_mult(p0, v1, v2, N);
   output(p0, N);
}
void vec_mult(float* &p1, float* &v3, float* &v4, int &N)
{
   for (int i=0; i<N; i++)
     p1[i] = v3[i] * v4[i];
}

