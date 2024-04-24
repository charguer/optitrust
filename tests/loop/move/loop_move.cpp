#include <optitrust.h>

void f() {
  __pure();

  int X = 6;
  int Y = 6;

  for (int cx = 0; cx < 2; cx++){
    __strict();
    for (int cy = 0; cy < 2; cy++){
      __strict();
      for (int bx = 2*cx; bx < X; bx += 4){
        __strict();
        for (int by = 2*cy; by < Y; by += 4){
          __strict();
          for (int x = bx; x < bx+2; x++){
            __strict();
            for (int y = by; y < by+2; y++){
              __strict();
              int x = 6;
            }
          }
        }
      }
    }
  }

  for (int y = 0; y < 10; y++) {
    __strict();
  }

  // return 0;
}
