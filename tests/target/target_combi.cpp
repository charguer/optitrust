typedef struct { int x; int y; } vect;

int f(int n) {
  for (int i = 0; i < 10; i++) {
    for (int j = 0; j < 12; j++) {
       i++;
       j++;
       int k = j;
    }
  }
  return 3;
}

int main() {
  for (int i = 0; i < 3; i+ +) {
    vect r = { f(2), f(3) };
  }
  for (int j = 0; j < 3; j++) {
    if (true) {
      j++;
    } else {
      j++;
    }
  }
}