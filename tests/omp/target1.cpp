void init(float*, float*, int);
void output(float*, int);
void vec_mult(int N)
{
   float p[N], v1[N], v2[N];
   init(v1, v2, N);
   for (int i=0; i < N; i++)
     p[i] = v1[i] * v2[i];
   output(p, N);
}
