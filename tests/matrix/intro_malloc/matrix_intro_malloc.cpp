#include <stdlib.h>
#include "../../include/optitrust.h"
const int N = 10;
int* q;

void allocate (){
 q = (int*) malloc(N * sizeof(int));
}

int main () {
  int* p = (int*) malloc (N * sizeof(int));
  return 0;
}