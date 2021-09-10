int const N = 10000;

int const M = 1024;

#pragma omp declare target

float Q[N][N];

#pragma omp declare simd uniform(i) linear(k) notinbranch

float P(int const i, int const k) { return (Q[i][k] * Q[k][i]); }

#pragma omp end declare target

float accum() {
  float tmp = 0.;
#pragma omp target map(tofrom : tmp)
#pragma omp parallel for reduction(+ : tmp)
  for (int i = 0; (i < N); i++) {
    float tmp1 = 0.;
#pragma omp parallel for reduction(+ : tmp1)
    for (int k = 0; (k < M); k++) {
      tmp1 += P(i, k);
    }
    tmp += tmp1;
  }
  return tmp;
}