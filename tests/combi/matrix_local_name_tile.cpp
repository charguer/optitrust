
#include "../../include/optitrust.h"
typedef int T;
// FIXME: not supported yet
// T* b;

int main() {
  const int N0 = 5;
  const int N1 = 10;
  const int N2 = 10;
  const int N3 = 10;
  // TODO: deal with CALLOC
  T* a = (T*) MALLOC3 (N1, N2, N3, sizeof(T));
  for (int i = 0; i < 10; i++) {
    for (int j = 2; j < 10; j++) {
      for (int k = 0; k < 4; k++) {
        a[MINDEX3(N1,N2,N3,i,j,k)] = 1;
      }
    }
  }
  free(a);

  T* b = (T*) MALLOC3 (N1, N2, N3, sizeof(T));
  for (int i = 0; i < 10; i++) {
    for (int j = 2; j < 10; j++) {
      for (int k = 0; k < 4; k++) {
        b[MINDEX3(N1,N2,N3,i,j,k)] = 1;
      }
    }
  }
  free(b);

  int z = 0;
  return 0;
}