typedef struct { int x; int y; } vect;

int f(int n) {
  for (int i = 0; i < 10; i++) {
    for (int j = 0; j < 12; j++) {
        for (int k = 0; k < 13; k++) {
          i++;
          j++;
          k++;
          i = k + j;
        }
    }
  }
  return 3;
}

int main() {
  for (int i = 0; i < 3; i++) {
    vect r = { f(2), f(3) };
  }
}
