int N;

int *t;

int BLOCK;

int main() {
  for (int i1 = 0; (i1 < (N / BLOCK)); i1++) {
    for (int i2 = 0; (i2 < BLOCK); i2++) {
      int i = ((i1 * BLOCK) + i2);
      t[i] = (i1 + i2);
    }
  }
}
