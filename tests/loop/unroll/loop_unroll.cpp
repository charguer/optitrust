const int N = 3;
const int M = 5;
int s = 2;
const int u = 2;

int main() {
  for (int i = s; i < s + N; i++) {
    int a = 1;
    int b = a + 2;
    int c = 3;
    int d = c + 4;
    int e = d + 5;
  }

  for (int j = u; j < M; j++){
    int x = j;
    int y = j + 1;
  }

  for (int k = 0; k < 2; k++) {
    for (int k2 = 0; k2 < 2; k2++) {
      s = k + k2;
    }
  }

  return 0;
}
