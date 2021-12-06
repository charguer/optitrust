
#include "../../include/optitrust.h"

typedef int T;
int main(){
  const int N0 = 5;
  const int N1 = 10;
  const int N2 = 10;
  const int N3 = 10;
  T* a = (T*) MCALLOC3 (N1, N2, N3, sizeof(T));
  for (int i = 0; i < 10; i++) {
    int t = a[MINDEX3(N1,N2,N3,i,i+1,i+2)];
    a[MINDEX3(N1,N2,N3,i,i+1,i+2)] = t + 5;
  }
  int y = 0;
  return 0;
}