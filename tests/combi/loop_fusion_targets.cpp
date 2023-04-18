int* t;
int* u;
int* v;
int n;

int main() {
  float p = 5.0;
  for (int i = 0; i < n; i++) {
    t[i] = i;
  }
  int x = 1;
  for (int i = 0; i < n; i++) {
    u[i] += i;
  }
  for (int k0 = 0; k0 < n; k0++) {
    for (int k1 = 0; k1 < n; k1++) {
      p += k0;
    }
  }
  int y = 2;
  for (int k0 = 0; k0 < n; k0++) {
    for (int k1 = 0; k1 < n; k1++) {
      p += k1;
    }
  }
  for (int j = 0; j < n; j++) {
    v[j] += j;
  }
  int a;
  int b;
  int c;
  return 0;
}

