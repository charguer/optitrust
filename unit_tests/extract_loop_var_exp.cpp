int *t;

int *u;

int main() {
  {
    int x[10];
    for (int i = 0; (i < 10); i++) {
      x[i] = t[i];
      u[i] = x[i];
    }
  }
}
