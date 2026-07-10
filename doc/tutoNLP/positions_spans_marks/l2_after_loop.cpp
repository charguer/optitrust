void work(int);
void finish();

void f(int n) {
  for (int i = 0; i < n; i++) {
    work(i);
  }
  finish();
}
