#include <stdio.h>

void work1() {}
void work2() {}

void single_example()
{
  {
    printf("Beginning work1.\n");
    work1();
    printf("Finishing work1.\n");
    work2();
    printf("Finishing work2.\n");
  }
}
