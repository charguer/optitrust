#include <optitrust.h>

void f() {
  __pure();

  int x = 0;
  for (int i = 0; i < 10; i++){
    x += i;
  }
  const int cut = 2;
  for (int k = 0; k < 10; k++){
    x += k;
  }

}
