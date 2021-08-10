#include <stdio.h>
int main() {
   int x = 1;
   {
      x = 2;
      printf("x = %d\n", x);
   }
   return 0;
}
