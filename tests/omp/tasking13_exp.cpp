#include <string.h>

#include <omp.h>

int const LIMIT = 3;

void check_solution(char *);

void bin_search(int pos, int n, char *state) {
#pragma omp task final(pos > LIMIT) mergeable
  if ((pos == n)) {
    check_solution(state);
    return;
  }
  {
    char new_state[n];
    if ((!omp_in_final())) {
      memcpy(new_state, state, pos);
      state = new_state;
    }
    state[pos] = 0;
    bin_search((pos + 1), n, state);
  }
#pragma omp task final(pos > LIMIT) mergeable
  {
    char new_state[n];
    if ((!omp_in_final())) {
      memcpy(new_state, state, pos);
      state = new_state;
    }
    state[pos] = 1;
    bin_search((pos + 1), n, state);
  }
#pragma omp taskwait
}