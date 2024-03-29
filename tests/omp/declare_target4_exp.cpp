const int N = 10000;

#pragma omp declare target
float Q[N][N];

float Pfun(const int i, const int k) { return Q[i][k] * Q[k][i]; }

#pragma omp end declare target
float accum(int k) {
  float tmp = 0.;
#pragma omp target update to(Q)
#pragma omp target map(tofrom : tmp)
#pragma omp parallel for reduction(+:tmp )
  for ( int i = 0; i < N; i++ ) tmp += Pfun(i, k);
    return tmp;
}
