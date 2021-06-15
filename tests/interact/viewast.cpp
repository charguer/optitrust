
typedef struct { int x; int y; } vect;

typedef struct { vect pos; vect speed; } particle;

int f(int x) {
  x = x + 1;
  return x;
}


void test_loop() {
  for (int i = 0; i < 10; i++) {
      i++;
  }
}

int main() {
   const int n = f(43);
   return 0;
}


