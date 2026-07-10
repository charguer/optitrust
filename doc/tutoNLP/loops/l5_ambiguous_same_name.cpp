void clear(int);
void update(int);

void init(int n) {
  for (int i = 0; i < n; i++) {
    clear(i);
  }
}

void step(int n) {
  for (int i = 0; i < n; i++) {
    update(i);
  }
}
