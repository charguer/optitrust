int *t;

int main() {
  int C = 2;
  int D = 2;
  for (int ci = 0; (ci < C); ci++) {
    for (int i = (ci * D); (i < 20); i += (C * D)) {
      t[i] = 0;
    }
  }
  for (int cj = 0; (cj < C); cj++) {
    for (int j = cj; (j < 20); j += C) {
      t[j] = 0;
    }
  }
  int B = 2;
  int X = 20;
  for (int x = 0; (x < X); x++) {
    t[x] = 0;
  }
}
