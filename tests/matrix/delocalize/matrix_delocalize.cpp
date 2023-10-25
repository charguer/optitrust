
#include "../../../include/optitrust.h"

typedef int T;

T* b;

const int N0 = 5;
const int N1 = 10;
const int N2 = 10;
const int N3 = 10;

void allocate () {

  b = (T*) CALLOC3 (N1, N2, N3, sizeof(T));
}

int main(){

  T* a = (T*) CALLOC3 (N1, N2, N3, sizeof(T));
  for (int i = 0; i < 10; i++) {
    int t = a[MINDEX3(N1,N2,N3,i,i+1,i+2)];
    a[MINDEX3(N1,N2,N3,i,i+1,i+2)] = t + 5;
  }

  for (int j = 0; j < 10; j++) {
    b[MINDEX3(N1,N2,N3,j,j+1,j+2)];
  }

  int z = 0;
  return 0;
}
