

int main() {
  for (int i = 0; i < 5; i++) {
    i++;
  }
  for (int i = 0; i < 5; i++) {
    i++;
  }
  int j = 0;
  int r = 0;
  for (j = 1, r = 2; j + r < 5; j++, r--) {
    j++;
  }
  for (int i = 0, k = 2; i < 5; i++, j++, k--) {
    i++;
  }
  for (; ; ) {
  }
}


// gcc -std=c99 c_small_test.cpp
// gcc -std=c++11 c_small_test.cpp