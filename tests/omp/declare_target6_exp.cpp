const int N = 100000000;

#pragma omp declare target link(sp, sv1, sv2) link(dp, dv1, dv2)
float sp[N];

float sv1[N];

float sv2[N];

double dp[N];

double dv1[N];

double dv2[N];

void s_init(float*, float*, int);

void d_init(double*, double*, int);

void s_output(float*, int);

void d_output(double*, int);

#pragma omp declare target
void s_vec_mult_accum() {
  int i;
#pragma omp parallel for
  for (i = 0; i < N; i++) sp[i] = sv1[i] * sv2[i];
}

void d_vec_mult_accum() {
  int i;
#pragma omp parallel for
  for (i = 0; i < N; i++) dp[i] = dv1[i] * dv2[i];
}

#pragma omp end declare target
int main() {
  s_init(sv1, sv2, N);
#pragma omp target map(to : sv1, sv2) map(from : sp)
  s_vec_mult_accum();
  s_output(sp, N);
  d_init(dv1, dv2, N);
#pragma omp target map(to : dv1, dv2) map(from : dp)
  d_vec_mult_accum();
  d_output(dp, N);
  return 0;
}
