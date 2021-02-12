typedef int T[2][3];

int get_0_1 (int t[2][3]) {
  return t[0][1];
}

int get_1_2 (int t[2][3], int v[2][3]) {
  return t[1][2] + get_0_1(v) + get_0_1(t);
}

int main () {
  T t;
  int v[2][3];

  for (int i = 0; i < 2; i++) {
    for (int j = 0; j < 3; j++) {
      t[i][j] = 2 * i + j;
    }
  }

  test:
    int n = t[1][2] + get_1_2(t, v) + get_1_2(v, t) + get_1_2(t, t) +
      get_1_2(v, v);

  return 0;
}
