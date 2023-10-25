
#include "../../../include/optitrust.h"

int main (){
  // TODO: deal with CALLOC
  int* const a = (int* const) MALLOC1(10, sizeof(int));
  for (int i = 3; i < 7; i++) {
    a[MINDEX1(10, i)];
  }
  MFREE1(10, a);
}
