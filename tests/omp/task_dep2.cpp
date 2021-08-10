#include <stdio.h>
int main()
{
   int x = 1;
   {
      printf("x = %d\n", x);
      x = 2;
   }
}
