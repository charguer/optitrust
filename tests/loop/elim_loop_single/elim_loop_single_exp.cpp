int main() {
  const int k = 10;
  const int i = k;
  int j = i + k;
}

int main2() {
  const int k = 10;
  const int i = k;
  int j = i + k;
  const int j1 = k + 1;
  return 0;
}

int main3() {
  for (int i = 0; i < 2; i++) {
    int j = i + 1;
  }
  return 0;
}
