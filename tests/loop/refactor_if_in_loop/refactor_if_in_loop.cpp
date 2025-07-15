// one condition

int main() {
  const int k = 10;
  for (int i = 0; i < 16; i++) {
    if (i > k) {
      const int j = 1;
    }
  }
}
// multiples inequalities

int main2() {
  const int k = 10;
  for (int i = 0; i < 16; i++) {
    if (i > k && i >= k - 3 && i < k + 10) {
      const int j = 1;
    }
  }
  return 0;
}

// left and right side term
int main3 (){
  const int k = 10;
  for (int i = 0; i < 16; i++) {
    if (k<= i  && i < k-4) {
      const int j = 1;
    }
  }
  return 0;
}

//
int main4 (){
  const int k = 10;
  for (int i = 0; i < 16; i++) {
    if (k<= i  && i < k-4) {
      const int j = 1;
    }
  }
  return 0;
}
