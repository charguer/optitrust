int* t;

int main() {
  for (int a = 0; a < 7; a++) {
    for (int b = 0; b < 10; b++) {
      for (int c = 0; c < 20; c++) {
        t[a] = b;
      }
    }
  }
  for (int i = 0; i < 10; i++) {
    for (int j = i; j < i + 1; j++) {
    }
  }
}
