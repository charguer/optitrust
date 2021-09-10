float dotprod(float B[], float C[], int N) {
  float sum = 0;
#pragma omp target teams map(to : B [0:N], C [0:N]), defaultmap(tofrom : scalar)
#pragma omp distribute parallel for reduction(+ : sum)
  for (int i = 0; (i < N); i++)
    sum += (B[i] * C[i]);
  return sum;
}