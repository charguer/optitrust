void load(int);
void store(int);

void two_passes(int n) {
  for (int i = 0; i < n; i++) {
    load(i);
  }
  for (int i = 0; i < n; i++) {
    store(i);
  }
}
