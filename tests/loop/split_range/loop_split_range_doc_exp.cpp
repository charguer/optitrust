int main() {
  int x = 0;
  for (int i = 0; i < i + 2; i++) {
    x += i;
  }
  for (int i = i + 2; i < 10; i++) {
    x += i;
  }
  int cut = 2;
  for (int k = 0; k < cut; k++) {
    x += k;
  }
  for (int k = cut; k < 10; k++) {
    x += k;
  }
}
