#include <assert.h>

void priv_example3() {
  int a;
#pragma omp parallel private(a)
  {
    a = 1;
#pragma omp parallel for private(a)
    for (int i = 0; (i < 10); i++) {
      a = 2;
    }
  }
}