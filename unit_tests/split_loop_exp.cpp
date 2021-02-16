int *t;

int *u;

int n;

int main() {
  {
    for (int i = 1; (i < n); i++) {
      t[i] = i;
    }
    for (int i = 1; (i < n); i++) {
      u[i] += i;
    }
  }
}
