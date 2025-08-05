int main() {
  const int k = 10;
  for (int i = max(k + 1, 0); i < 16; i++) {
    const int j = 1;
  }
}

int main2() {
  const int k = 10;
  for (int i = max(k - 3, max(k + 1, 0)); i < min(k + 10, 16); i++) {
    const int j = 1;
  }
  return 0;
}

int main3() {
  const int k = 10;
  for (int i = max(k, 0); i < min(k - 4, 16); i++) {
    const int j = 1;
  }
  return 0;
}

int main4() {
  const int k = 10;
  for (int i = max(k, 0); i < min(k - 4, 16); i++) {
    const int j = 1;
  }
  return 0;
}
