int ANY(int maxValue) { return 0; }

int const N = 2;

typedef int T;

T CHOOSE(int nb, T b1, T b2) { return b1; }

int main() {
  T a;
  T xa;
  T xb;
  xa = a;
  xb = 0;
  for (int i = 0; (i < 2); i++) {
    CHOOSE(2, xa, xb)++;
  }
  a = xa;
  a += xb;
  int y = 0;
  return 0;
}