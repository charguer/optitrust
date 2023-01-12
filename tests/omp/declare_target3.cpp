const int N =1000;
float p[N], v1[N], v2[N];
extern void init(float *, float *, int);
extern void output(float *, int);
void vec_mult()
{
   int i;
   init(v1, v2, N);
   for (i=0; i<N; i++)
     p[i] = v1[i] * v2[i];
   output(p, N);
}
