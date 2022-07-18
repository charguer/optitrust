#include <string.h>
#include <omp.h>
#include <stdio.h>

const int LIMIT = 3; /* arbitrary limit on recursion depth */

void check_solution(char *p);
void bin_search (int pos, int n, char *state)
{
   if ( pos == n ) {
      check_solution(state);
      return;
   }
   {
      char new_state[n];
      if (!omp_in_final() ) {
        memcpy(new_state, state, pos );
        state = new_state;
      }
      state[pos] = 0;
      bin_search(pos+1, n, state );
   }
   {
      char new_state[n];
      if (! omp_in_final() ) {
        memcpy(new_state, state, pos );
        state = new_state;
      }
      state[pos] = 1;
      bin_search(pos+1, n, state );
   }
}
