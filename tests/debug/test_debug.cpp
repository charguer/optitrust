
#include "../../include/optitrust.h"

const int N0 = 5;
const int N1 = 10;
const int N2 = 10;
const int N3 = 10;


typedef int T;
T* b;

void allocate (){
  b = (T*) MCALLOC3 (N1, N2, N3, sizeof(T));
  int x = 10;
  x++;
}

int main(){
  

  for (int j = 0; j < 10; j++) {
    b[MINDEX3(N1,N2,N3,j,j+1,j+2)];
  }

  int z = 0;
  return 0;
}