#include <omp.h>

int dequeue(float *a);
void work(int i, float *a);

void critical_example(float *x, float *y)
{
  int ix_next, iy_next;

  {
    ix_next = dequeue(x);
    work(ix_next, x);

    iy_next = dequeue(y);
    work(iy_next, y);
  }

}
