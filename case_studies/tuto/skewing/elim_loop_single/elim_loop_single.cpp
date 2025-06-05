
int main() {
  for (int k = 0; k < 0 + 1; k++) {
    const int j = 12 + k;
  }
}
// Expected to fail : No for loop with +1 increment
int main2() {

  for (int k = 10; k < 25; k++) {
    int j = 12;
  }
  return 12;
}
