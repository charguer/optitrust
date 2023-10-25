int main() {
  int x, y, z;
  for (int i = 0; i < 10; i++) {
    for (int j = 0; j < 10; j++) {
      x = i;
      y = i;
    }
  }
  for (int i = 0; i < 10; i++) {
    for (int j = 0; j < 10; j++) {
      z = i;
    }
  }
  for (int i = 0; i < 10; i++) {
    for (int k = 0; k < 10; k++) {
      x = k;
    }
  }
  for (int i = 0; i < 10; i++) {
    for (int k = 0; k < 10; k++) {
      y = k;
    }
  }
}
