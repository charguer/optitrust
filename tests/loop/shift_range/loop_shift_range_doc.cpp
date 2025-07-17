#include <optitrust.h>

void f() {
  int x = 0;
  for (int i = 2; i < 12; i++){
    x += i;
  }
  const int shift = 2;
  for (int k = 0; k < 10; k++){
    x += k;
  }
}
