

const int N = 10;
const int M = 20;

typedef int T[N][M];

T t;

int main() {
  int i = 0;
  int j = 1;
  t[i][j] = 4;
  t[2][3] = 3 + t[0][1];
}


