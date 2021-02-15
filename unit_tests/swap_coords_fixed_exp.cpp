

const int N = 10;
const int M = 20;

typedef int T;

T t[M][N];

int main() {
  int i = 0;
  int j = 1;
  t[j][i] = 4;
  t[3][2] = 3 + t[1][0];
}

