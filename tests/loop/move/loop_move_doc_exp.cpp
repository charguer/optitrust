int main() {
  int x = 0;
  for (int k = 0; k < 10; k++) {
    for (int i = 0; i < 10; i++) {
      for (int j = 0; j < 10; j++) {
        x = i + j + k;
      }
    }
  }
}
