int main() {
  int x = 0;
  const int st = 0;
  const int N = 10;
  for (int i = 0; i < i + 5; i++) {
    x += i;
  }
  for (int i = i + 5; i < 10; i++) {
    x += i;
  }
  for (int j = st; j < j + 5; j++) {
    x += j;
  }
  for (int j = j + 5; j < N; j++) {
    x += j;
  }
  int cut = 5;
  for (int k = 0; k < cut; k++) {
    x += k;
  }
  for (int k = cut; k < N; k++) {
    x += k;
  }
  for (int l = st; l < cut; l++) {
    x += l;
  }
  for (int l = cut; l < N; l++) {
    x += l;
  }
}
