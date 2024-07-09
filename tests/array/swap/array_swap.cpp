typedef int T[2][3];

int get_1_x(int a[2][3]) {
  return a[0][1];
}

int get_1_y(T a) {
  return a[0][1];
}

int get_2_x(int a[2][3], int b[2][3]) {
  return a[1][2] + b[1][2];
}

int get_2_y(T a, T b) {
  return a[1][2] + b[1][2];
}



int main () {
  T t;
  int v[2][3]; // should not be changed

  for (int i = 0; i < 2; i++) {
    for (int j = 0; j < 3; j++) {
      t[i][j] = 2 * i + j;
    }
  }

  int n = t[1][2] + get_1_x(v) + get_1_y(t) + get_2_x(v, v) + get_2_y(t, t);

  return 0;
}
