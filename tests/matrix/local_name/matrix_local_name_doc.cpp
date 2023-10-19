
#include "../../../include/optitrust.h"
typedef int T;
T* b;

int main (){

  const int N0 = 1;
  T* a = (T*) CALLOC1(N0, sizeof(T));
  for (int i = 0; i < 10; i++){
    a[MINDEX1(N0, i)];
  }
}
