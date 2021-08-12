#include <stdio.h>

int main (void)
{
  int a;

  {
    a = 0;

    // To avoid race conditions, add a barrier here.
    for (int i = 0; i < 10; i++) {
        a += i;
    }

    printf ("Sum is %d\n", a);
  }
  return 0;
}