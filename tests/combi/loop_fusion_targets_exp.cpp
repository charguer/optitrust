int* t;

int* u;

int* v;

int n;

int main() {
  float p = 5.f;
  for (int i = 0; i < n; i++) {
    t[i] = i;
    u[i] += i;
    v[i] += i;
  }
  int x = 1;
  for (int k0 = 0; k0 < n; k0++) {
    for (int k1 = 0; k1 < n; k1++) {
      p += k0;
      p += k1;
    }
  }
  int y = 2;
  int a;
  int b;
  int c;
  return 0;
}
