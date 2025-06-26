//  Index used inside the loop
int main() {
  const int k = 10;
  for (int i = k; i < k + 1; i++) {
    int j = i + k;
  }
}
// Instruction after the loop
int main2() {
  const int k = 10;
  for (int i = k; i < k + 1; i++) {
    int j = i + k;
  }
  const int j = k + 1;
  return 0;
}

// More than one loop iteration
int main3() {
  for (int i = 0; i < 2; i++) {
    int j = i + 1;
  }
  return 0;
}
