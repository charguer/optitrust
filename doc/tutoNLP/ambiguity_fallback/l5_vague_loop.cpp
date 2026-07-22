void a(int);
void b(int);

void f(int n) {
  for (int i = 0; i < n; i++) {
    a(i);
  }
  for (int j = 0; j < n; j++) {
    b(j);
  }
}
