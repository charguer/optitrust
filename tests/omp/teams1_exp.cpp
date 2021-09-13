#include <stdlib.h>

#include <omp.h>

float dotprod(float B[], float C[], int N) {
  float sum0 = 0.;
  float sum1 = 0.;
#pragma omp target map(to : B[:N], C[:N]), map(tofrom : sum0, sum1)
#pragma omp taskloop num_teams(2)
  {
    if ((omp_get_num_teams() != 2))
      abort();
    if ((omp_get_team_num() == 0)) {
#pragma omp parallel for reduction(+ : sum0)
      for (int i = 0; (i < (N / 2)); i++)
        sum0 += (B[i] * C[i]);
    } else if ((omp_get_team_num() == 1)) {
#pragma omp parallel for reduction(+ : sum1)
      for (int i = (N / 2); (i < N); i++)
        sum1 += (B[i] * C[i]);
    }
  }
  return (sum0 + sum1);
}