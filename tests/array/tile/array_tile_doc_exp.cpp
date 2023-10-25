const int B = 8;

typedef int BLOCK[B];

typedef BLOCK* T;

int main() {
  T t;
  int a = t[3 / B][3 % B];
}
