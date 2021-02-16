const int B = 8;

typedef int X[B];
typedef X *T;
T t;

typedef int Y[B];
typedef Y U[(80 / B)];
U u;

int main() {
  int i;
  int x = t[(i / B)][(i % B)];
  int y = u[(i / B)][(i % B)];
}
