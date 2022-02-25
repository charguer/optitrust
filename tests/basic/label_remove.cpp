#include <stdbool.h>

int main() {

  int x;
  start:
   x = 3;

  loop: for (int i = 0; i < 3; i++) {

  cond: if (1) {
    incr_1: x++;
  } else {
    incr_2: x--;
    }
  }

   stop: return 0;
  }

