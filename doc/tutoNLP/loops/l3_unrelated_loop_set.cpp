void prepare(int);
void compute(int);
void finish(int);

void stages(int n) {
  for (int p = 0; p < n; p++) {
    prepare(p);
  }
  for (int q = 0; q < n; q++) {
    compute(q);
  }
  for (int r = 0; r < n; r++) {
    finish(r);
  }
}
