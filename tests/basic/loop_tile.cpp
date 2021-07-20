int* t;

int min(int x, int y) {
  return (x < y) ? x : y;
}

int main() {

  for (int x = 0; x < 10; x += 3) {
    t[x] = 0;
  }
  for (int y = 0; y < 10; y += 3) {
    t[y] = 0;
  }
  return 0;
}