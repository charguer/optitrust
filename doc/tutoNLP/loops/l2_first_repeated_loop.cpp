void horizontal(int);
void vertical(int);

void blur(float* out) {
  for (int y = 0; y < 10; y++) {
    horizontal(y);
  }
  for (int y = 0; y < 10; y++) {
    vertical(y);
  }
}
