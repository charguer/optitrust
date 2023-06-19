
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

  T* c = (T*) MALLOC3 (N1, N2, N3, sizeof(T));
  for (int i = 0; i < 10; i++) {
    for (int j = 2; j < 10; j++) {
      for (int k = 0; k < 4; k++) {
        c[MINDEX3(N1,N2,N3,i,j,k)] = 1;
      }
    }
    for (int j2 = 2; j2 < 10; j2++) {
      for (int k2 = 0; k2 < 4; k2++) {
        c[MINDEX3(N1,N2,N3,i,j2,k2)] = 2;
      }
    }
  }
  free(c);

  int z = 0;
  return 0;
}