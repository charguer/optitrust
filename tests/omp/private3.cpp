#include <assert.h>
void priv_example3()
{
  int a;

  {
      a = 1;
      for (int i=0; i<10; i++)
     {
       a = 2;
     }
    // assert(a == 1);
  }
}
