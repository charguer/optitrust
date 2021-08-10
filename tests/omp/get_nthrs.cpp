#include <omp.h>
void work(int i);

void correct()
{
  int i;

  {
    work(i);
  }
}

