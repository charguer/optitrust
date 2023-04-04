#include <stdio.h>
#include "../../include/optitrust.h"

extern int N;
typedef float T;

void main() {
  T a[N];
  for (int i = 0; i < N; i++) {
    a[i] = i;
  }

  T b[N - 2];
  for (int i = 0; i < N - 2; i++) {
    b[i] = a[i] + a[i + 1] + a[i + 1];
  }

  for (int i = 0; i < N - 4; i++) {
    printf("%i\n", b[i] + b[i + 1] + b[i + 2]);
  }
}

/* storage folding transformations:

# circular buffering for 'a'

T a[3];
for (int i = 0; i < 2; i++) {
  a[i] = i;
}

T b[N - 2];
for (int i = 0; i < N - 2; i++) {
  a[(i + 2) % 3] = i + 2;
  b[i] = a[(i + 0) % 3] + a[(i + 1) % 3] + a[(i + 2) % 3];
}

for (int i = 0; i < N - 4; i++) {
  printf("%i\n", b[i] + b[i + 1] + b[i + 2]);
}

# register rotation for 'b'

T a[3];
for (int i = 0; i < 2; i++) {
  a[i] = i;
}

a[2 % 3] = 2;
T b0 = a[0 % 3] + a[1 % 3] + a[2 % 3];
a[3 % 3] = 3;
T b1 = a[1 % 3] + a[2 % 3] + a[3 % 3];
T b2;

for (int i = 0; i < N - 4; i++) {
  a[(i + 4) % 3] = i + 2;
  b2 = a[(i + 2) % 3] + a[(i + 3) % 3] + a[(i + 4) % 3];
  printf("%i\n", b1 + b2 + b3);
  b0 = b1;
  b1 = b2;
}

*/