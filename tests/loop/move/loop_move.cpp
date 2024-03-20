#include <optitrust.h>

void f() {
  __pure();

  int X = 6;
  int Y = 6;

  for (int cx = 0; cx < 2; cx++){
  __pure();
     for (int cy = 0; cy < 2; cy++){
      __pure();
       for (int bx = 2*cx; bx < X; bx += 4){
        __pure();
         for (int by = 2*cy; by < Y; by += 4){
         __pure();
           for (int x = bx; x < bx+2; x++){
            __pure();
             for (int y = by; y < by+2; y++){
                __pure();
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
