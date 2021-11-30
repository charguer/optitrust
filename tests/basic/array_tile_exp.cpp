int const B = 8;

typedef int T_BLOCK[B];

typedef T_BLOCK *T;

T t;

typedef int U_BLOCK[B];

typedef U_BLOCK *U;

U u;

typedef int V_BLOCK[B];

typedef V_BLOCK V[(80 / B)];

V v;

int const N = 40;

int w[(N / B)][B];

int main() {
  int i;
  int x = t[(i / B)][(i % B)];
  int y = u[(i / B)][(i % B)];
  int z = v[(i / B)][(i % B)];
  for (i = 0; (i < N); i++) {
    w[(i / B)][(i % B)] = 0;
  }
}
