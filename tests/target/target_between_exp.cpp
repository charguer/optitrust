#include "stdbool.h"

typedef struct {
  int x;
  int y;
} vect;

/*@ 16_0 @*/

int main() {
  for (int i = 0; i < 10; i++) {
    /*@ 9_0, 1_0 @*/
    int r1 = 1;
    /*@ 5_0, 2_0 @*/
    int r2 = 2;
    /*@ 6_0 @*/
    if (true) {
      /*@ 3_0 @*/
      int m1 = 1;
      /*@ 7_0, 4_0 @*/
      int m2 = 2;
      /*@ 15_0, 13_0, 8_0 @*/
    } else {
      /*@ 10_0 @*/
      int s1 = 1;
      int s2 = 2;
      /*@ 14_0, 11_0 @*/
    }
    /*@ 12_0 @*/
  }
}

/*@ 17_0 @*/
