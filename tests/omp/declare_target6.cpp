const int N  = 100000000;

float  sp[N], sv1[N], sv2[N];
double dp[N], dv1[N], dv2[N];

void s_init(float *, float *, int);
void d_init(double *, double *, int);
void s_output(float *, int);
void d_output(double *, int);

void s_vec_mult_accum()
{
   for (int i=0; i<N; i++)
     sp[i] = sv1[i] * sv2[i];
}

void d_vec_mult_accum()
{
   for (int i=0; i<N; i++)
     dp[i] = dv1[i] * dv2[i];
}

int main()
{
   s_init(sv1, sv2, N);
   s_vec_mult_accum();
   s_output(sp, N);

   d_init(dv1, dv2, N);
   d_vec_mult_accum();
   d_output(dp, N);

  return 0;
}
