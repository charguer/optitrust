#include <optitrust.h>
const int N = 5000;
void reduction_reinit(double* A, double* sum){
 *sum = 0;
 double x[1];
 x[0] = *sum;
 int nb_threads = 10;
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      x[0] += A[MINDEX2(N,N,i,j)];
      A[MINDEX2(N,N,i,j)] = 0.;
    }
  }
 *sum = x[0];
}
