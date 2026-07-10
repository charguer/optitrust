void work(int);

void kernel(int n) {
  for (int i = 0; i < n; i++) {
    work(i);
  }
}
