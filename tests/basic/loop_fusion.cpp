int* t;
int* u;
int* v;
int n;

int main() {
  float p = 5.0;
  tofusion: {
    for (int i = 0; i < n; i++) {
      t[i] = i;
    }
    for (int i = 0; i < n; i++) {
      u[i] += i;
    }
    for (int i = 0; i < n; i++) {
      v[i] += i;
    }
  }
  return 0;
}
