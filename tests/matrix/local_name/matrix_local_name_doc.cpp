#include <optitrust.h>

typedef int T;

int main (){

  const int N0 = 1;
  T* const a = (T* const) CALLOC1(N0, sizeof(T));
  for (int i = 0; i < 10; i++){
    a[MINDEX1(N0, i)];
  }
}
