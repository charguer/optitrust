int *t;

int *u;

int n;

int main() {
  for (int i = 1; (i < n); i++) {
    int a = i;
  }
  for (int i = 1; (i < n); i++) {
    t[i] += a;
  }
  for (int i = 1; (i < n); i++) {
    int b = i;
  }
  for (int i = 1; (i < n); i++) {
    u[i] += b;
  }
  return 0;
}
