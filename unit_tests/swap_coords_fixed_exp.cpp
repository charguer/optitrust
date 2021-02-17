const int N = 10;

const int M = 20;

typedef int T[20][10];

T t;

int main() {
  int i = 0;
  int j = 1;
  t[j][i] = 4;
  t[3][2] = (3 + t[1][0]);
}
