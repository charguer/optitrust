#include <stdlib.h>
#include "../../include/optitrust.h"

int* q;

const int N = 10;

void allocate (){
 q = (int*) calloc(N, sizeof(int));
}


int main () {

  int* p = (int*) calloc(N, sizeof(int));
  return 0;
}