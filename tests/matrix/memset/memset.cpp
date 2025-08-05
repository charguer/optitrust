#include <optitrust.h>

int main() {
const int N = 1;
const int M = 3;
double x[N*M];

  for (int i = 0; i < N; i++) {
    for (int j = 0; j < M; j++) {
    x[MINDEX2(N,M,i,j)] = 25;
    }
  }
  for (int i = 0; i < N*M; i++) {
    x[MINDEX1(N*M,i)] = 25;
  }
  // M*N instead of N*M
  for (int k = 0; k < N*M; k++) {
    x[MINDEX1(M*N,k)] = 25;
  }
   for (int k = 1; k < N*M; k++) {
    x[MINDEX1(N*M,k)] = 25;
  }
   for (int k = 0; k < N*M-1; k+=2) {
    x[MINDEX1(N*M,k)] = 25;
  }
   for (int k = 0; k < N*M; k+=2) {
    k++;
  }
  // todo : rajouter les erreurs
}
