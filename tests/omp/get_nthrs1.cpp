#include <omp.h>
void work(int i);

void incorrect(){
  int np, i;

  for (i = 0; i < np; i++){
    work(i);
  }
}
