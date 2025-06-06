#include <optitrust.h>
const int N = 2000;

void matmat_kernel(double C[N*N], double A[N*N], double B[N*N]) {
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      C[MINDEX2(N,N,i,j)] = 0.;  //
      for (int k = 0; k < N; k++) {
        C[MINDEX2(N,N,i,j)] += A[MINDEX2(N,N,i,k)] * B[MINDEX2(N,N,k,j)];
      }
    }
  }
}
