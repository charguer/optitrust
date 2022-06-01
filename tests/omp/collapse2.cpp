#include <stdio.h>
void test()
{
  int jlast;
  int klast;
  for (int k=1; k<=2; k++)
     for (int j=1; j<=3; j++)
     {
        jlast=j;
        klast=k;
     }
  printf("%d %d\n", klast, jlast);
}
