int main() {
  const int t[2] = { 1, 2 };
  int a = t[0] + t[1];
  int b = t[1] * 2;

  const int t2[2] = { 3, 4 };
  for (int i = 0; i < 2; i++) {
    for (int j = 0; j < 1; j++) {
      a = t2[i + j];
    }
  }
}