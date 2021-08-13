const int THRESHOLD1 = 1000000;

const int THRESHOLD2 = 1000;

void init(float*, float*, int);
void output(float*, int);

void vec_mult(float *p, float *v1, float *v2, int N)
{
   init(v1, v2, N);
   for (int i=0; i<N; i++)
     p[i] = v1[i] * v2[i];
   output(p, N);
}
