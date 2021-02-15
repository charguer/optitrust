
int N;
int* t;

int BLOCK; // assumes BLOCK DIVIDES N

int main() {
  for (int i1 = 0; i1 < N/BLOCK; i1++) {
    for (int i2 = 0; i2 < BLOCK; i2++) {
      int i = i1 * BLOCK + i2;
      t[i] = i1 + i2;
    }
  }
}

// NOTE: the current implementation of loop tiling removes 'int i' in case
// it is not used in the loop.

