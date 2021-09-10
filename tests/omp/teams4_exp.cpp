int const N = (1024 * 1024);

float dotprod(float B[], float C[]) {
  float sum = 0.;
#pragma omp target teams map(to : B [0:N], C [0:N]), map(tofrom : sum)
#pragma omp taskloop num_teams(8) thread_limit(16) reduction(+ : sum)
#pragma omp distribute parallel for reduction(+:sum)  dist_schedule(static, 1024)  schedule(static, 64)
  for (int i = 0; (i < N); i++)
    sum += (B[i] * C[i]);
  return sum;
}