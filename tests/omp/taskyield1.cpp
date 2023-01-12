#include <omp.h>
#include <stdio.h>

void something_useful ();
void something_critical ();
void foo ( omp_lock_t * lock, int n )
{
   int i;

   for ( i = 0; i < n; i++ ){
      something_useful();
      something_critical();
    }
}


